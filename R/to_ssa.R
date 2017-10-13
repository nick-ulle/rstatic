
#' Convert CFGraph to Static Single-Assignment Form
#'
#' This function converts code in a control flow graph (CFG) to static
#' single-assignment form.
#'
#' @param node (Function) A function with CFG attached.
#' @param inPlace (logical) Don't copy CFG before conversion?
#' 
#' @return The function with its control flow graph converted to static
#' single-assignment form.
#'
#' @export
toSSA = function(node, inPlace = FALSE) {
  # TODO: make this function's implementation more idiomatic.
  if (!inherits(node, "Function") || is.null(node$cfg))
    stop("node must be a Function in CFG form. Use toCFG() to convert.")

  if (!inPlace)
    node = node$copy()

  cfg = node$cfg

  cb = collectCrossblockUses(cfg)
  uses = cb[[1]]
  assign_blocks = cb[[2]]

  entry_idx = cfg$get_index(cfg$entry)
  dom_t = domTree(cfg, entry_idx)
  dom_f = domFrontier(cfg, dom_t)

  # Insert phi-functions.
  for (name in uses) {
    # Add phi-function to dominance frontier for each block with an assignment.
    worklist = assign_blocks[[name]]
    while (length(worklist) > 0) {
      b = worklist[[1]]
      worklist = worklist[-1]

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

  ssaRenameAST(node$params, builder)
  ssaRename(entry_idx, cfg, dom_t, builder)

  node$ssa = builder$ssa
  node$global_uses = builder$global_uses

  node
}


#' Rename CFG Variables with SSA Names
#'
#' This function renames variables in the basic blocks of a CFG with their SSA
#' names.
#'
#' Generally, this function should only be called from \code{toSSA()}.
#'
#' @param block (integer) Index of a basic block in the CFG.
#' @param cfg (CFGraph) A control-flow graph.
#' @param dom_t (integer) The dominator tree for the CFG.
#' @param builder (SSABuilder) A stateful object used by the renaming
#'
ssaRename = function(block, cfg, dom_t, builder) {
  # Rewrite LHS of phi-functions in this block.
  ssaRenameAST(cfg[[block]]$phi, builder)

  ssaRenameAST(cfg[[block]]$body, builder)

  # Rewrite terminator in this block.
  term = cfg[[block]]$terminator
  if (inherits(term, "CondBrTerminator")) {
    ssaRenameAST(term$condition, builder)
  } else if (inherits(term, "RetTerminator")) {
    ssaRenameAST(term$value, builder)
  }

  # Rewrite RHS of phi-functions in successors.
  block_name = cfg$get_name(block)
  for (i in neighbors(cfg$graph, block, "out")) {
    lapply(cfg[[i]]$phi, function(phi) {
      n = builder$get_live_def(phi$write$basename)
      node = Symbol$new(phi$write$basename, n)

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
    })
  }

  # Descend to blocks dominated by this block (children in dom tree).
  builder$save_local_defs()

  children = setdiff(which(dom_t == block), block)
  lapply(children, ssaRename, cfg, dom_t, builder)

  # End lifetimes of variables defined in this block.
  builder$clear_local_defs()
}


#' Rename AST Variables with SSA Names
#'
#' This function renames variables in an AST with their SSA names.
#'
#' Generally, this function should only be called from \code{ssaRename()}.
#'
#' @param node (ASTNode) An abstract syntax tree.
#' @param builder (SSABuilder) A stateful object used by the renaming
#' algorithm.
#'
ssaRenameAST = function(node, builder) {
  # FIXME: This doesn't change function names.
  UseMethod("ssaRenameAST")
}

#' @export
ssaRenameAST.Assign = function(node, builder) {
  builder$register_uses = FALSE
  ssaRenameAST(node$read, builder)
  builder$register_uses = TRUE

  node$write$ssa_number = builder$new_def(node$write$basename)
  builder$register_def(node$write$name, node)

  node
}

#' @export
ssaRenameAST.Phi = function(node, builder) {
  node$write$ssa_number = builder$new_def(node$write$basename)
  builder$register_def(node$write$name, node)

  node
}

#' @export
ssaRenameAST.Parameter = function(node, builder) {
  if (!is.null(node$default))
    ssaRenameAST(node$default, builder)

  node$ssa_number = builder$new_def(node$basename)
  # FIXME: Parameter processing order might not put all defs before uses.
  builder$register_def(node$name, node)

  node
}

#' @export
ssaRenameAST.Call = function(node, builder) {
  lapply(node$args, ssaRenameAST, builder)

  ssaRenameAST(node$fn, builder)

  node
}

#' @export
ssaRenameAST.Function = function(node, builder) {
  toSSA(node, inPlace = TRUE)

  node
}

# NOTE:
# ssaRenameAST.Replacement() is now handled by ssaRenameAST.Assign()

#' @export
ssaRenameAST.Brace = function(node, builder) {
  lapply(node$body, ssaRenameAST, builder)

  node
}

#' @export
ssaRenameAST.Symbol = function(node, builder) {
  node$ssa_number = builder$get_live_def(node$basename)

  if (builder$register_uses) {
    # FIXME: This should register the use on the line of code that contains the
    # node, which might not always be the parent.
    builder$register_use(node$name, node$parent)
  }

  node
}

#' @export
ssaRenameAST.Literal = function(node, builder)
  node

#' @export
ssaRenameAST.list = function(node, builder) {
  lapply(node, ssaRenameAST, builder)
  node
}
