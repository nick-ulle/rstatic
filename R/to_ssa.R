
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

  dom_t = dom_tree(cfg)
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
  ssa_rename(cfg$entry, cfg, dom_t)

  return (cfg)
}


#' Rename CFG Variables with SSA Names
#'
#' This function renames variables in the basic blocks of a CFG with their SSA
#' names.
#'
#' Generally, this function should only be called from \code{to_ssa()}.
#'
#' @param block (integer) Identifier of a basic block in the CFG.
#' @param cfg (CFGraph) A control-flow graph.
#' @param dom_t (integer) The dominator tree for the CFG.
#' @param ns (NameStack) A stateful object used by the renaming algorithm.
#'
ssa_rename = function(block, cfg, dom_t, ns = NameStack$new()) {
  # Rewrite function arguments in this block (if any).
  if (inherits(cfg[[block]], "FnEntryBlock")) {
    ssa_rename_ast(cfg[[block]]$params, ns)
  }
  
  # Rewrite LHS of phi-functions in this block.
  ssa_rename_ast(cfg[[block]]$phi, ns)

  # Rewrite operations in this block.
  ssa_rename_ast(cfg[[block]]$body, ns)

  # Rewrite terminator in this block.
  term = cfg[[block]]$terminator
  if (inherits(term, "BranchInst") && !is.null(term$condition)) {
    ssa_rename_ast(term$condition, ns)
  } else if (inherits(term, "ReturnInst")) {
    ssa_rename_ast(term$value, ns)
  }
  # NOTE: for-loop iterators already appear in the body because of how
  # for-loops get translated to CFGs.

  #else if (inherits(term, "IterateInst")) {
  #  ssa_rename_ast(term$iter, ns)
  #}

  # Rewrite RHS of phi-functions in successors.
  for (id in cfg[[block]]$successors) {
    lapply(cfg[[id]]$phi, function(phi) {
      name = ns$get_name(phi$base)
      phi$set_read(block, name)
    })
  }

  # Descend to blocks dominated by this block (children in dom tree).
  ns$save_locals()

  children = setdiff(which(dom_t == block), block)
  lapply(children, ssa_rename, cfg, dom_t, ns)

  # End lifetimes of variables defined in this block.
  ns$clear_locals()
}


#' Rename AST Variables with SSA Names
#'
#' This function renames variables in an AST with their SSA names.
#'
#' Generally, this function should only be called from \code{ssa_rename()}.
#'
#' @param node (ASTNode) An abstract syntax tree.
#' @param ns (NameStack) A stateful object used by the renaming algorithm.
#'
ssa_rename_ast = function(node, ns) {
  # FIXME: This doesn't change function names.
  # Rename all AST elements.
  UseMethod("ssa_rename_ast")
}

#' @export
ssa_rename_ast.Assign = function(node, ns) {
  ssa_rename_ast(node$read, ns)
  node$write$name = ns$new_name(node$write$name)
  return (node)
}

#' @export
ssa_rename_ast.Phi = function(node, ns) {
  node$write = ns$new_name(node$base)
  return (node)
}

#' @export
ssa_rename_ast.Parameter = function(node, ns) {
  node$name = ns$new_name(node$name)
  if (!is.null(node$default))
    ssa_rename_ast(node$default, ns)
  return (node)
}

#' @export
ssa_rename_ast.Call = function(node, ns) {
  lapply(node$args, ssa_rename_ast, ns)
  return (node)
}

#' @export
ssa_rename_ast.Brace = function(node, ns) {
  lapply(node$body, ssa_rename_ast, ns)
  return (node)
}

#' @export
ssa_rename_ast.Symbol = function(node, ns) {
  node$name = ns$get_name(node$name)
  return (node)
}

#' @export
ssa_rename_ast.Literal = function(node, ns) return (node)

#' @export
ssa_rename_ast.list = function(node, ns) {
  lapply(node, ssa_rename_ast, ns)
  return (node)
}
