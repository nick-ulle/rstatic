
#' Compute Dominator Sets
#'
#' This function computes the dominator sets for each block in a control-flow
#' graph.
#'
#' A block \eqn{b_i} dominates another block \eqn{b_j} when all program paths
#' to \eqn{b_j} must pass through \eqn{b_i}. The dominance property is
#' reflexive: every block dominates itself. This function iteratively computes
#' the set of dominators for each block until a fixed point is reached. In most
#' cases, only 2-3 iterations are necessary.
#'
#' @param cfg (CFGraph) a control-flow graph
#'
#' @export
dominators = function(cfg) {
  rpo = rev(cfg$get_postorder())

  dom = vector("list", length(cfg))
  dom[rpo] = list(rpo)

  # The only dominator for block 1 is itself.
  dom[[1]] = 1
  rpo = rpo[-1]

  changed = TRUE
  while (changed) {
    changed = FALSE

    for (i in rpo) {
      preds = cfg[[i]]$predecessors
      temp = union(i, Reduce(intersect, dom[preds]))

      if (!identical(temp, dom[[i]])) {
        dom[[i]] = temp
        changed = TRUE
      }
    } # end for
  }

  return (dom)
}
