
#' Compute Dominator Tree
#'
#' This function computes the dominator tree for a control-flow graph.
#'
#' A block \eqn{b_i} dominates another block \eqn{b_j} when all program paths
#' to \eqn{b_j} must pass through \eqn{b_i}. The dominance property is
#' reflexive: every block dominates itself. This function iteratively computes
#' the immediate dominator for each block until a fixed point is reached.
#'
#' @param cfg (CFGraph) A control-flow graph.
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
dom_tree = function(cfg) {
  po = cfg$get_postorder()
  rpo = rev(po)
  # FIXME: Assign postorder number to each block instead of using a postorder
  # traversal so that less indirection is necessary here.
  # Map (block index -> postorder number) for later use.
  lookup = order(po, decreasing = FALSE)

  doms = integer(length(cfg))

  doms[[1]] = rpo[[1]]
  rpo = rpo[-1]

  # Iterate until a fixed point is reached.
  changed = TRUE
  while (changed) {
    changed = FALSE

    for (i in rpo) {
      # Get predecessors of block i with entries in the dominator tree.
      preds = cfg[[i]]$predecessors
      preds = preds[doms[preds] != 0]

      # Walk up the dominator tree to find a common dominator for predecessors
      # of block i.
      new_idom = preds[[1]]
      for (j in preds[-1]) {
        b1 = j
        b2 = new_idom
        while(b1 != b2) {
          while (lookup[[b1]] < lookup[[b2]])
            b1 = doms[[b1]]
          while (lookup[[b2]] < lookup[[b1]])
            b2 = doms[[b2]]
        }
        new_idom = b1
      }

      # Update dominator tree if necessary.
      if (doms[[i]] != new_idom) {
        doms[[i]] = new_idom
        changed = TRUE
      }
    }
  }

  return (doms)
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
#' @param cfg (CFGraph) A control-flow graph.
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
    preds = cfg[[i]]$predecessors
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
