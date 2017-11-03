#' Convert CFGraph to Static Single-Assignment Form
#'
#' This function converts code in a control flow graph (CFG) to static
#' single-assignment form.
#'
#' @param node (Function) A function with CFG attached.
#' @param in_place (logical) Don't copy CFG before conversion?
#' 
#' @return The function with its control flow graph converted to static
#' single-assignment form.
#'
#' @export
to_ssa = function(node, in_place = FALSE) {
  # TODO: make this function's implementation more idiomatic.
  if (!inherits(node, "Function") || is.null(node$cfg))
    stop("node must be a Function in CFG form. Use to_cfg() to convert.")

  if (!in_place)
    node = node$copy()

  cfg = node$cfg

  cb = collect_crossblock_uses(cfg)
  uses = cb[[1]]
  assign_blocks = cb[[2]]

  entry_idx = cfg$get_index(cfg$entry)
  dom_t = dominator_tree(cfg, entry_idx)
  dom_f = dominator_frontier(cfg, dom_t)

  # Insert phi-functions.
  for (name in uses) {
    # Add phi-function to dominance frontier for each block with an assignment.
    worklist = assign_blocks[[name]]
    while (length(worklist) > 0) {
      b = worklist[[1]]
      worklist = worklist[-1]

      for (d in dom_f[[b]]) {
        if (name %in% names(cfg[[d]]$phi))
          next

        phi = Phi$new(name)
        cfg[[d]]$set_phi(phi)
        worklist = union(worklist, d)
      } # end for d
    }
  } # end for name

  # Rename variables.
  builder = SSABuilder$new()

  ssa_rename_ast(node$params, builder)
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

  # Rewrite LHS of phi-functions in this block.
  ssa_rename_ast(cfg[[block]]$phi, builder)

  ssa_rename_ast(cfg[[block]]$body, builder)

  # Rewrite RHS of phi-functions in successors.
  block_name = cfg$get_name(block)
  for (i in neighbors(cfg$graph, block, "out")) {
    lapply(cfg[[i]]$phi, function(phi) {
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
    })
  }

  # Descend to blocks dominated by this block (children in dom tree).
  children = setdiff(which(dom_t == block), block)
  lapply(children, ssa_rename, cfg, dom_t, builder)

  # Restore defs from parent block.
  builder$defs = parent_defs
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
  UseMethod("ssa_rename_ast")
}

#' @export
ssa_rename_ast.If = function(node, builder) {
  ssa_rename_ast(node$condition, builder)
}

#' @export
ssa_rename_ast.For = function(node, builder) {
  ssa_rename_ast(node$ivar, builder)
  ssa_rename_ast(node$iter, builder)
}

#' @export
ssa_rename_ast.While = function(node, builder) {
  ssa_rename_ast(node$condition, builder)
}

#' @export
ssa_rename_ast.Assign = function(node, builder) {
  builder$register_uses = FALSE
  ssa_rename_ast(node$read, builder)
  builder$register_uses = TRUE

  node$write$ssa_number = builder$new_def(node$write$basename)
  builder$register_def(node$write$name, node$write$basename, node)

  node
}

#' @export
ssa_rename_ast.Phi = function(node, builder) {
  node$write$ssa_number = builder$new_def(node$write$basename)
  builder$register_def(node$write$name, node$write$basename, node)

  node
}

#' @export
ssa_rename_ast.Parameter = function(node, builder) {
  if (!is.null(node$default))
    ssa_rename_ast(node$default, builder)

  node$ssa_number = builder$new_def(node$basename)
  # FIXME: Parameter processing order might not put all defs before uses.
  builder$register_def(node$name, node$basename, node)

  node
}

#' @export
ssa_rename_ast.Application = function(node, builder) {
  lapply(node$args, ssa_rename_ast, builder)
  node
}

#' @export
ssa_rename_ast.Call = function(node, builder) {
  NextMethod()
  ssa_rename_ast(node$fn, builder)
  node
}

#' @export
ssa_rename_ast.Function = function(node, builder) {
  to_ssa(node, in_place = TRUE)

  node
}

# NOTE:
# ssa_rename_ast.Replacement() is now handled by ssa_rename_ast.Assign()

#' @export
ssa_rename_ast.Brace = function(node, builder) {
  lapply(node$body, ssa_rename_ast, builder)

  node
}

#' @export
ssa_rename_ast.Symbol = function(node, builder) {
  node$ssa_number = builder$get_live_def(node$basename)

  if (builder$register_uses) {
    # FIXME: This should register the use on the line of code that contains the
    # node, which might not always be the parent.
    builder$register_use(node$name, node$parent)
  }

  node
}

#' @export
ssa_rename_ast.Literal = function(node, builder)
  node

#' @export
ssa_rename_ast.list = function(node, builder) {
  lapply(node, ssa_rename_ast, builder)
  node
}
