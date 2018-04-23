
to_ssa_new = function(node) {
  # Compute liveness.
  killgen = lapply(node$line, live_variables_killgen, list())
  killgen = purrr::transpose(killgen)

  #node$gen = killgen$gen
  node$kill = killgen$kill

  cfg = extract_cfg(node)

  dom_t = dominator_tree(cfg)
  dom_f = dominator_frontier(cfg, dom_t)
  names(dom_f) = names(V(cfg))

  # Where is each variable defined?
  defs = node$kill # list of defs per block
  blocks = rep(seq_along(defs), sapply(defs, length))
  blocks = node$block[blocks]

  flat_defs = unlist(defs)
  names = unique(flat_defs) # all def'd names

  phi_locations = lapply(names, function(name) {
    # Get the defs we know about.
    len = -1
    phis = character(0)
    defs = blocks[name == flat_defs]

    # Loop while the phi set is changing.
    while (length(phis) != len) {
      len = length(phis)

      # Append dominance frontiers for the defs set.
      # Where symbol is live.
      new_phis = unlist(dom_f[defs])
      new_phis = names(dom_f)[new_phis]
      # FIXME: Filter out non-live.
      phis = union(phis, new_phis)

      # The new phis are defs, so we need to visit them.
      defs = new_phis
    }

    phis
  })
  names(phi_locations) = names

  browser()

  # Then need to actually set the phi functions...

  # Finally, rename the variables
}

#' Convert CFGraph to Static Single-Assignment Form
#'
#' This function converts code in a control flow graph (CFG) to static
#' single-assignment form.
#'
#' This function has side effects. A CFG holds references to abstract syntax
#' tree (AST) nodes. As a result, there is no simple way to copy a CFG without
#' breaking the AST. This function always modifies CFGs in place.
#'
#' @param node (Function) A function with CFG attached.
#' 
#' @return The function with its control flow graph converted to static
#' single-assignment form.
#'
to_ssa = function(node) {
  # TODO: make this function's implementation more idiomatic.
  if (!is(node, "Function") || is.null(node$cfg))
    stop("node must be a Function in CFG form. Use to_cfg() to convert.")

  cfg = node$cfg

  # Run live variables analysis to generate pruned SSA.
  liveness = live_variables(cfg, full_analysis = TRUE)
  live = liveness[["entry"]]

  # Get the kill set for each block to figure out where definitions are.
  # NOTE: This assumes the analysis returns blocks in the same order as their
  # indices.
  definitions = list()
  for (b in rownames(liveness)) {
    for (name in liveness[["kill"]][[b]]) {
      idx = cfg$get_index(b)
      definitions[[name]] = append(definitions[[name]], idx)
    }
  }

  # For semi-pruned SSA, uses is the union of the gen sets.

  entry_idx = cfg$get_index(cfg$entry)
  dom_t = dominator_tree(cfg, entry_idx)
  dom_f = dominator_frontier(cfg, dom_t)

  # Insert phi-functions.
  for (symbol in names(definitions)) {
    # Add phi-function to dominance frontier for each block with an assignment.
    worklist = definitions[[symbol]]

    while (length(worklist) > 0) {
      b = worklist[[1]]
      worklist = worklist[-1]

      for (frontier in dom_f[[b]]) {
        # Do nothing if phi-function is already present.
        if (symbol %in% names(cfg[[frontier]]$phi))
          next

        # Check if variable is live at entry to the frontier.
        if (symbol %in% live[[frontier]]) {
          phi = Phi$new(symbol)
          cfg[[frontier]]$set_phi(phi)
          # The phi-function is a definition, so add frontier to worklist.
          worklist = union(worklist, frontier)
        }
      } # end for frontier
    }
  } # end for symbol

  # Rename variables.
  builder = SSABuilder$new()

  ssa_rename_ast(node$params, builder, record_uses = TRUE)
  ssa_rename(entry_idx, cfg, dom_t, builder)

  node$ssa = builder$ssa

  node
}


#' Rename CFG Variables with SSA Names
#'
#' This function renames variables in the basic blocks of a CFG with their SSA
#' names.
#'
#' Generally, this function should only be called from \code{to_ssa()}.
#'
#' @param block (integer) Index of a basic block in the CFG.
#' @param cfg (CFGraph) A control-flow graph.
#' @param dom_t (integer) The dominator tree for the CFG.
#' @param builder (SSABuilder) A stateful object used by the renaming
#' algorithm.
#'
ssa_rename = function(block, cfg, dom_t, builder) {
  # Save defs from parent block.
  parent_defs = builder$defs

  # Set SSA numbers in this block.
  ssa_rename_ast(cfg[[block]]$phi, builder, record_uses = TRUE)
  ssa_rename_ast(cfg[[block]]$body, builder, record_uses = TRUE)

  # Set SSA numbers for USES in phi-functions of immediate successors.
  block_name = names(cfg)[[block]]
  for (i in successors(block_name, cfg)) {
    lapply(cfg[[i]]$phi, set_ssa_successor_phis, builder, block_name)
  }

  # Set SSA numbers in blocks dominated by this block.
  children = setdiff(which(dom_t == block), block)
  lapply(children, ssa_rename, cfg, dom_t, builder)

  # Restore defs from parent block.
  builder$defs = parent_defs
}


set_ssa_successor_phis = function(phi, builder, block_name) {
  n = builder$get_live_def(phi$write$basename)
  node = Symbol$new(phi$write$basename, n)

  # FIXME: The design for Phis could be better.
  phi$set_read(block_name, node)

  # Add a backedge if the phi-function's LHS has been renamed already.
  #
  # NOTE: The second check is in case the read symbol is a global; globals
  # are not included in the SSA graph. Globals appear in phi-functions when
  # the definition of a symbol is conditional, e.g.,
  #
  #   if ( ... )
  #     x = 3
  #
  # Minimal SSA form prevents extraneous phi-functions from being generated
  # when the symbol is only live inside the body of the conditional.
  if (!is.na(phi$write$ssa_number) && !is.na(node$ssa_number))
    # TODO: This should be in the builder API.
    builder$ssa$add_edge(node$name, phi$write$name)
}


#' Rename AST Variables with SSA Names
#'
#' This function renames variables in an AST with their SSA names.
#'
#' Generally, this function should only be called from \code{ssa_rename()}.
#'
#' @param node (ASTNode) An abstract syntax tree.
#' @param builder (SSABuilder) A stateful object used by the renaming
#' algorithm.
#'
ssa_rename_ast = function(node, builder, record_uses) {
  UseMethod("ssa_rename_ast")
}

# Definitions --------------------

#' @export
ssa_rename_ast.Assign = function(node, builder, record_uses) {
  # NOTE: This method also handles Replacement objects.
  ssa_rename_ast(node$read, builder, record_uses = FALSE)

  node$write$ssa_number = builder$new_def(node$write$basename)
  builder$register_def(node$write$name, node$write$basename, node)

  node
}

#' @export
ssa_rename_ast.Phi = function(node, builder, record_uses) {
  node$write$ssa_number = builder$new_def(node$write$basename)
  builder$register_def(node$write$name, node$write$basename, node)

  node
}

#' @export
ssa_rename_ast.Parameter = function(node, builder, record_uses) {
  if (!is.null(node$default))
    ssa_rename_ast(node$default, builder, record_uses)

  # FIXME: Parameter processing order might not put all defs before uses.
  node$ssa_number = builder$new_def(node$basename)
  builder$register_def(node$name, node$basename, node)

  node
}

#' @export
ssa_rename_ast.For = function(node, builder, record_uses) {
  ssa_rename_ast(node$iterator, builder, record_uses = FALSE)

  node$variable$ssa_number = builder$new_def(node$variable$basename)
  builder$register_def(node$variable$name, node$variable$basename, node)

  node
}


# Uses --------------------

#' @export
ssa_rename_ast.Symbol = function(node, builder, record_uses) {
  node$ssa_number = builder$get_live_def(node$basename)

  if (record_uses)
    # FIXME: Register on the line, which might not be the parent.
    builder$register_use(node$name, node$parent)

  node
}

#' @export
ssa_rename_ast.Function = function(node, builder, record_uses) {
  to_ssa(node)
  node
}


# Other Stuff --------------------

#' @export
ssa_rename_ast.If = function(node, builder, record_uses) {
  ssa_rename_ast(node$condition, builder, record_uses)
  node
}

#' @export
ssa_rename_ast.While = ssa_rename_ast.If


#' @export
ssa_rename_ast.Application = function(node, builder, record_uses) {
  lapply(node$args, ssa_rename_ast, builder, record_uses)
  node
}

#' @export
ssa_rename_ast.Call = function(node, builder, record_uses) {
  NextMethod()
  ssa_rename_ast(node$fn, builder, record_uses)
  node
}

#' @export
ssa_rename_ast.Brace = function(node, builder, record_uses) {
  lapply(node$body, ssa_rename_ast, builder, record_uses)

  node
}

#' @export
ssa_rename_ast.Literal = function(node, builder, record_uses)
  node

#' @export
ssa_rename_ast.list = function(node, builder, record_uses) {
  # NOTE: So that lapply() is not needed every time `ssa_rename_ast()` is
  # called on a list.
  lapply(node, ssa_rename_ast, builder, record_uses)
  node
}
