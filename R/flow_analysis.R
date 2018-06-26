#' Compute Fixed Point Solution for Backward Analysis
#'
#' This function solves for the maximal fixed point in a backward analysis.
#'
#' @param cfg (ControlFlowGraph) The CFG to compute the analysis for.
#' @param initial Initial guess of solution for each block.
#' @param gen Gen sets for each block.
#' @param kill Kill sets for each block.
#' @param confluence A function to combine old set and update set for a block.
#' @param update A function to compute the update set for a block.
#' @param ... Additional arguments to the update function.
#' @param max_iter (integer) Maximum number of iterations to run.
#' @param full_analysis (logical) If \code{TRUE}, the update sets are computed
#' in addition to the result sets.
#'
#' @return A two-element list. The first element, "entry", is \code{NULL}, or
#' for a full analysis, a list of solution sets at the entry to each block. The
#' second element, "exit", is a list of solution sets at the exit from each
#' block.
#'
#' @export
backward_analysis =
function(cfg, initial, gen, kill
  , confluence = union
  , update = dfa_standard_update
  , ..., full_analysis = FALSE, max_iter = 1000L)
{
  # Step 1: Initialize worklist of edges.
  # TODO: Sort worklist so blocks are bottom to top.
  worklist = igraph::ends(cfg, igraph::E(cfg))

  # Step 2: Iterate until worklist is empty.
  iter = 0L
  while (nrow(worklist) > 0 && iter < max_iter) {
    # For backward analysis, destination vertex comes first.
    b      = worklist[1, 2]
    b_next = worklist[1, 1]
    worklist = worklist[-1, , drop = FALSE]

    # Update result set for the next block.
    old = initial[[b_next]]
    update_set = update(initial[[b]], gen[[b]], kill[[b]], ...)
    new = confluence(old, update_set)

    if (length(old) != length(new)) {
      initial[[b_next]] = new

      # Add edges from b_next to ancestors.
      in_edges = igraph::incident(cfg, b_next, "in")
      to_bind = igraph::ends(cfg, in_edges)
      worklist = rbind(to_bind, worklist)
    }

    iter = iter + 1L
  }

  # Step 3: The entry sets can be computed by an update() on each exit set.
  entry = if (full_analysis) {
    Map(update, initial, gen, kill)
  }

  list(entry = entry, exit = initial)
}


#' Standard Update for Data Flow Analyses
#'
#' This function computes the standard update set for data flow analyses. The
#' standard update set is the result set less the kill set, all unioned with
#' the gen set.
#'
#' @param result The result set for the current block.
#' @param gen The gen set for the current block.
#' @param kill The kill set for the current block.
#' @param ... Additional optional arguments to the update function.
#'
#' @return The update set for the next block.
dfa_standard_update = function(result, gen, kill, ...) {
  union(setdiff(result, kill), gen)
}


