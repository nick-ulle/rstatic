#' Compute Fixed Point Solution for Backward Analysis
#'
#' This function solves for the maximal fixed point in a backward analysis.
#'
#' @param cfg (ControlFlowGraph) The CFG to compute the analysis for.
#' @param initial Initial guess for result set for each block.
#' @param killgen Kill and gen sets for each block.
#' @param update A function to compute the update set for a block.
#' @param ... Additional arguments to the update function.
#' @param max_iter (integer) Maximum number of iterations to run.
#' @param full_analysis (logical) If \code{TRUE}, the update sets are computed
#' in addition to the result sets.
#'
#' @param (list) A list that contains the result sets and (in a full analysis)
#' the update sets.
#'
#' @export
backward_analysis =
function(cfg, initial, killgen, confluence = union,
  update = dfa_standard_update, ...,
  full_analysis = FALSE, max_iter = 1000L)
{
  # Step 1: Initialize worklist.
  # TODO: Sort worklist so blocks are bottom to top.
  worklist = igraph::ends(cfg$graph, igraph::E(cfg$graph))

  # Step 2: Iterate until worklist is empty.
  i = 0L
  while (nrow(worklist) > 0 && i < max_iter) {
    # For backward analysis, destination vertex comes first.
    b      = worklist[1, 2]
    b_next = worklist[1, 1]
    worklist = worklist[-1, , drop = FALSE]

    # Update result set for the next block.
    old = initial[[b_next]]
    update_set = update(initial[[b]], killgen[[b]], ...)
    new = confluence(old, update_set)

    if (length(old) != length(new)) {
      initial[[b_next]] = new

      # Add edges from b_next to ancestors.
      in_edges = igraph::incident(cfg$graph, b_next, "in")
      to_bind = igraph::ends(cfg$graph, in_edges)
      worklist = rbind(to_bind, worklist)
    }

    i = i + 1L
  }

  # Step 3: If needed, the entry sets can be computed by calling analysis() on
  # each block.
  if (full_analysis) {
    update_sets = lapply(names(initial), function(name) {
      update(initial[[name]], killgen[[name]])
    })
    names(update_sets) = names(initial)
  } else {
    update_sets = NULL
  }

  list(result = initial, update = update_sets)
}


#' Standard Update for Data Flow Analyses
#'
#' This function computes the standard update set for data flow analyses. The
#' standard update set is the result set less the kill set, all unioned with
#' the gen set.
#'
#' @param result The result set for the current block.
#' @param killgen The kill and gen sets for the current block.
#'
#' @return The update set for the next block.
dfa_standard_update = function(result, killgen, ...) {
  union(setdiff(result, killgen[["kill"]]), killgen[["gen"]])
}


