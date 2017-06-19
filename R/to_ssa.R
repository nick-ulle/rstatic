
#' Convert CFGraph to Static Single-Assignment Form
#'
#' This function converts code in a control flow graph (CFG) to static
#' single-assignment form.
#'
#' @param cfg (CFGraph) A control flow graph.
#' @param in_place (logical) Don't copy CFG before conversion?
#'
#' @return The control flow graph as a CFGraph object, with the code in each
#' block converted to SSA form.
#'
#' @export
to_ssa = function(cfg, in_place = FALSE) {
  # TODO: make this function's implementation more idiomatic.

  if (!in_place)
    cfg = cfg$copy()

  cb = collect_crossblock_uses(cfg)
  uses = cb[[1]]
  assign_blocks = cb[[2]]

  entry_idx = cfg$get_index(cfg$entry)
  dom_t = dom_tree(cfg, entry_idx)
  dom_f = dom_frontier(cfg, dom_t)

  # Insert phi-functions.
  for (name in uses) {
    # Add phi-function to dominance frontier for each block with an assignment.
    worklist = assign_blocks[[name]]
    for (b in worklist) {
      for (d in dom_f[[b]]) {
        if (has_phi(cfg[[d]], name))
          next

        phi = Phi$new(name)
        cfg[[d]]$append(phi)
        worklist = union(worklist, d)
      } # end for d
    }
  } # end for name

  # Rename variables.
  builder = SSABuilder$new()
  # TODO: Parameter renaming should happen in `ssa_rename()`, not here.
  ssa_rename_ast(cfg$params, builder)
  ssa_rename(entry_idx, cfg, dom_t, builder)

  cfg$ssa = builder$ssa

  return (cfg)
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
  # Rewrite LHS of phi-functions in this block.
  ssa_rename_ast(cfg[[block]]$phi, builder)

  ssa_rename_ast(cfg[[block]]$body, builder)

  # Rewrite terminator in this block.
  term = cfg[[block]]$terminator
  if (inherits(term, "CondBrTerminator")) {
    ssa_rename_ast(term$condition, builder)
  } else if (inherits(term, "RetTerminator")) {
    ssa_rename_ast(term$value, builder)
  }

  # Rewrite RHS of phi-functions in successors.
  block_name = cfg$get_name(block)
  for (i in neighbors(cfg$graph, block, "out")) {
    lapply(cfg[[i]]$phi, function(phi) {
      n = builder$get_live_def(phi$write$base)
      node = Symbol$new(phi$write$base, n)

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
      if (!is.na(phi$write$n) && !is.na(node$n))
        # TODO: This should be in the builder API.
        builder$ssa$add_edge(node$name, phi$write$name)
    })
  }

  # Descend to blocks dominated by this block (children in dom tree).
  builder$save_local_defs()

  children = setdiff(which(dom_t == block), block)
  lapply(children, ssa_rename, cfg, dom_t, builder)

  # End lifetimes of variables defined in this block.
  builder$clear_local_defs()
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
ssa_rename_ast = function(node, builder) {
  # FIXME: This doesn't change function names.
  UseMethod("ssa_rename_ast")
}

#' @export
ssa_rename_ast.Assign = function(node, builder) {
  builder$register_uses = FALSE
  ssa_rename_ast(node$read, builder)
  builder$register_uses = TRUE

  node$write$n = builder$new_def(node$write$base)
  builder$register_def(node$write$name, node)

  return (node)
}

#' @export
ssa_rename_ast.Phi = function(node, builder) {
  node$write$n = builder$new_def(node$write$base)
  builder$register_def(node$write$name, node)

  return (node)
}

#' @export
ssa_rename_ast.Parameter = function(node, builder) {
  if (!is.null(node$default))
    ssa_rename_ast(node$default, builder)

  node$n = builder$new_def(node$base)
  # FIXME: Parameter processing order might not put all defs before uses.
  builder$register_def(node$name, node)

  return (node)
}

#' @export
ssa_rename_ast.Call = function(node, builder) {
  lapply(node$args, ssa_rename_ast, builder)
  return (node)
}

#' @export
ssa_rename_ast.Brace = function(node, builder) {
  lapply(node$body, ssa_rename_ast, builder)
  return (node)
}

#' @export
ssa_rename_ast.Symbol = function(node, builder) {
  node$n = builder$get_live_def(node$base)

  if (builder$register_uses) {
    # FIXME: This should register the use on the line of code that contains the
    # node, which might not always be the parent.
    builder$register_use(node$name, node$parent)
  }

  return (node)
}

#' @export
ssa_rename_ast.Literal = function(node, builder)
  return (node)

#' @export
ssa_rename_ast.list = function(node, builder) {
  lapply(node, ssa_rename_ast, builder)
  return (node)
}
