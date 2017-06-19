
#' Preorder Traversal of a Tree
#'
#' Given a tree in vector form, this function finds the order nodes are visited
#' by a preorder traversal.
#'
#' @param dom_t (integer) The tree on which to compute a traversal, in vector
#' form. The value at each index should be the index of that node's parent.
#' @param from (integer) A node index where traversal should start.
#'
#' @return (integer) The node indexes in the order they'd be visited.
#'
#' @export
dom_tree_preorder = function(dom_t, from = 1L) {
  to_visit = Stack$new(type = "integer")
  to_visit$push(from)

  visited = integer(length(dom_t))
  n_visited = 0
  while (!to_visit$is_empty) {
    idx = to_visit$pop()
    n_visited = n_visited + 1
    visited[n_visited] = idx

    # Visit children of this node.
    children = setdiff(which(dom_t == idx), idx)
    to_visit$push_many(rev(children))
  }

  return (visited)
}

#' Compute Dominator Tree
#'
#' This function computes the dominator tree for a control-flow graph.
#'
#' A block \eqn{b_i} dominates another block \eqn{b_j} when all program paths
#' to \eqn{b_j} must pass through \eqn{b_i}. The dominance property is
#' reflexive: every block dominates itself. This function iteratively computes
#' the immediate dominator for each block until a fixed point is reached.
#'
#' @param cfg (ControlFlowGraph) A control-flow graph.
#' @param entry_idx (integer) Index of the root node of the graph.
#'
#' @return The dominator tree as a vector of immediate dominators. In other
#' words, if element \eqn{j} is \eqn{i}, then the immediate dominator of block
#' \eqn{j} is block \eqn{i}. The dominator set for block \eqn{j} consists of
#' all blocks on the path from block \eqn{j} to the root of the dominator tree.
#'
#' @references
#' Cooper, K. D., Harvey, T. J., and Kennedy, K. (2001) A simple, fast
#' dominance algorithm. Software Practice & Experience 4, 1-10.
#'
#' Cooper, K. D. and Torczon, L. (2012) Engineering a Compiler. Elsevier.
#'
#' @export
dom_tree = function(cfg, entry_idx = cfg$get_index(cfg$entry)) {
  dom_t = igraph::dominator_tree(cfg$graph, entry_idx)[["dom"]]
  dom_t = as.vector(dom_t)

  # Set entry as the immediate dominator of itself to avoid off-by-one indexes.
  dom_t = append(dom_t, entry_idx, after = entry_idx - 1L)

  return (dom_t)
}


#' Compute Dominance Frontiers
#'
#' This function computes the dominance frontiers for a control-flow graph.
#'
#' The dominance frontier for a block \eqn{b} is the set of all nodes \eqn{y}
#' such that \eqn{b} dominates are predecessor of \eqn{y} but does not strictly
#' dominate \eqn{y}. In other words, the dominance frontier for \eqn{b} is the
#' set of blocks immediately beyond the blocks dominated by \eqn{b}, where
#' control-flow merges from a separate part of the program.
#'
#' @param cfg (ControlFlowGraph) A control-flow graph.
#' @param dom_tree (integer) The dominator tree for the control-flow graph.
#' 
#' @return The dominance frontiers as a list of integer vectors. Each element
#' of the list is the dominance frontier for the corresponding block in the
#' control-flow graph.
#'
#' @references
#' Cooper, K. D., Harvey T. J., and Kennedy, K. (2001) A simple, fast dominance
#' algorithm. Software Practice & Experience 4, 1-10.
#'
#' Cooper, K. D. and Torczon, L. (2012) Engineering a Compiler. Elsevier.
#'
#' @export
dom_frontier = function(cfg, dom_tree) {
  dom_f = vector("list", length(cfg))
  dom_f[] = list(integer(0))

  for (i in seq_along(cfg)) {
    preds = igraph::neighbors(cfg$graph, i, "in")
    if (length(preds) > 1) {
      # Walk up dom tree for each predecessor
      for (j in preds) {
        runner = j
        while (runner != dom_tree[[i]]) {
          dom_f[[runner]] = union(dom_f[[runner]], i)
          runner = dom_tree[[runner]]
        }
      } # end for
    }
  } # end for

  return (dom_f)
}
