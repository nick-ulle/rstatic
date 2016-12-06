
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
