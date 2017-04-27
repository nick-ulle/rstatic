
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
  ns = NameStack$new()
  # TODO: Parameter renaming should happen in `ssa_rename()`.
  ssa_rename_ast(cfg$params, ns)
  ssa_rename(entry_idx, cfg, dom_t, ns)
  #cfg$ssa_graph = ns$ssa_graph
  browser() # FIXME:

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
#' @param ns (NameStack) A stateful object used by the renaming algorithm.
#'
ssa_rename = function(block, cfg, dom_t, ns = NameStack$new()) {
  # Rewrite LHS of phi-functions in this block.
  ssa_rename_ast(cfg[[block]]$phi, ns)

  ssa_rename_ast(cfg[[block]]$body, ns)

  # Rewrite terminator in this block.
  term = cfg[[block]]$terminator
  if (inherits(term, "IterTerminator")) {
    # FIXME: Conditions for for-loops should be generated with the CFG. Once
    # they are, this no-op case can be removed.

  } else if (inherits(term, "CondBrTerminator")) {
    ssa_rename_ast(term$condition, ns)
  } else if (inherits(term, "RetTerminator")) {
    ssa_rename_ast(term$value, ns)
  }

  # Rewrite RHS of phi-functions in successors.
  block_name = cfg$get_name(block)
  for (i in neighbors(cfg$graph, block, "out")) {
    lapply(cfg[[i]]$phi, function(phi) {
      n = ns$get_live_def(phi$write$base)
      node = Symbol$new(phi$write$base, n)
      ns$register_use(node$name, node)

      phi$set_read(block_name, node)
    })
  }

  # Descend to blocks dominated by this block (children in dom tree).
  ns$save_local_defs()

  children = setdiff(which(dom_t == block), block)
  lapply(children, ssa_rename, cfg, dom_t, ns)

  # End lifetimes of variables defined in this block.
  ns$clear_local_defs()
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
  UseMethod("ssa_rename_ast")
}

#' @export
ssa_rename_ast.Assign = function(node, ns) {
  ssa_rename_ast(node$read, ns)

  node$write$n = ns$new_def(node$write$base)
  ns$register_def(node$write$name, node)

  return (node)
}

#' @export
ssa_rename_ast.Phi = function(node, ns) {
  node$write$n = ns$new_def(node$write$base)
  ns$register_def(node$write$name, node)

  return (node)
}

#' @export
ssa_rename_ast.Parameter = function(node, ns) {
  if (!is.null(node$default))
    ssa_rename_ast(node$default, ns)

  node$n = ns$new_def(node$base)
  ns$register_def(node$name, node)

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
  node$n = ns$get_live_def(node$base)
  ns$register_use(node$name, node)

  return (node)
}

#' @export
ssa_rename_ast.Literal = function(node, ns) return (node)

#' @export
ssa_rename_ast.list = function(node, ns) {
  lapply(node, ssa_rename_ast, ns)
  return (node)
}
