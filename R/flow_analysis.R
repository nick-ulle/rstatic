#' Compute Fixed Point Solution for Forward Analysis
#'
#' This function solves for the maximal fixed point in a forward analysis.
#'
#' @param cfg (BlockList) A control flow graph with which to compute a
#' solution.
#' @param initial Initial guess of solution for each block.
#' @param gen Gen sets for each block.
#' @param kill Kill sets for each block.
#' @param confluence (function) A function to combine old set and update set
#' for a block.
#' @param update (function) A function to compute the update set for a block.
#' @param ... Additional arguments to `update`.
#' @param max_iter (integer) Maximum number of iterations to run.
#' @param full_analysis (logical) If `TRUE`, the update sets are computed in
#' addition to the result sets.
#'
#' @return A two-element list. The first element, "entry", is a list of
#' solution sets at the entry from each block. The second element, "exit", is
#' `NULL`, or for a full analysis, a list of solution sets at the exit to each
#' block.
#'
#' @seealso [backward_analysis()]
#' @export
forward_analysis =
function(cfg, initial, gen, kill
  , confluence = `|`
  , update = dfa_standard_update
  , ..., full_analysis = FALSE, max_iter = 1000L)
{
  # Step 1: Initialize worklist of edges.
  worklist = igraph::ends(cfg, igraph::E(cfg))

  # Step 2: Iterate until worklist is empty.
  iter = 0L
  while (nrow(worklist) > 0 && iter < max_iter) {
    # For forward analysis, source vertex comes first.
    b      = worklist[1, 1]
    b_next = worklist[1, 2]
    worklist = worklist[-1, , drop = FALSE]

    # Update result set for the next block.
    old = initial[, b_next]
    update_set = update(initial[, b], gen[, b], kill[, b], ...)
    new = confluence(old, update_set)

    # Check whether this iteration changed anything.
    if (sum(old) != sum(new)) {
      initial[, b_next] = new

      # Add edges from b_next to successors.
      out_edges = igraph::incident(cfg, b_next, "out")
      to_bind = igraph::ends(cfg, out_edges)
      worklist = rbind(to_bind, worklist)
    }

    iter = iter + 1L
  }

  # Step 3: The exit sets can be computed by an update() on each entry set.
  exit = if (full_analysis) {
    update(initial, gen, kill)
  } # else NULL

  list(entry = initial, exit = exit)
}


#' Compute Fixed Point Solution for Backward Analysis
#'
#' This function solves for the maximal fixed point in a backward analysis.
#'
#' @param cfg (BlockList) The CFG to compute the analysis for.
#' @param initial Initial guess of solution for each block.
#' @param gen Gen sets for each block.
#' @param kill Kill sets for each block.
#' @param confluence (function) A function to combine old set and update set
#' for a block.
#' @param update (function) A function to compute the update set for a block.
#' @param ... Additional arguments to `update`.
#' @param max_iter (integer) Maximum number of iterations to run.
#' @param full_analysis (logical) If `TRUE`, the update sets are computed in
#' addition to the result sets.
#'
#' @return A two-element list. The first element, "entry", is `NULL`, or for a
#' full analysis, a list of solution sets at the entry to each block. The
#' second element, "exit", is a list of solution sets at the exit from each
#' block.
#'
#' @seealso [forward_analysis()]
#' @export
backward_analysis =
function(cfg, initial, gen, kill
  , confluence = `|`
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
    old = initial[, b_next]
    update_set = update(initial[, b], gen[, b], kill[, b], ...)
    new = confluence(old, update_set)

    # Check whether this iteration changed anything.
    if (sum(old) != sum(new)) {
      initial[, b_next] = new

      # Add edges from b_next to ancestors.
      in_edges = igraph::incident(cfg, b_next, "in")
      to_bind = igraph::ends(cfg, in_edges)
      worklist = rbind(to_bind, worklist)
    }

    iter = iter + 1L
  }

  # Step 3: The entry sets can be computed by an update() on each exit set.
  entry = if (full_analysis) {
    update(initial, gen, kill)
  } # else NULL

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
#'
#' @export
dfa_standard_update = function(result, gen, kill, ...) {
  (result & !kill) | gen
}


#' Aggregate Data Flow Analysis Sets to Block-level
#'
#' This function takes line-level sets from a data flow analysis and aggregates
#' them to block-level by sequential application of the update function.
#'
#' @param gen (logical matrix) Gen sets for each line.
#' @param kill (logical matrix) Kill sets for each line.
#' @param forward (logical) Should sets be aggregated forward (first to last
#' line) or backward (last to first line)?
#' @param update (function) A function to compute the update set for a block.
#'
#' @return A logical vector with `nrow(gen)` elements. The set computed by
#' aggregating over all of the line-level sets.
#'
#' @export
block_aggregate =
function(gen, kill, forward = TRUE, update = dfa_standard_update)
{
  n = ncol(gen)
  if (forward) {
    index = seq(1L, n)
    initial = gen[, 1L]
  } else {
    index = seq(n, 1L)
    initial = gen[, n]
  }

  for (j in index) {
    initial = update(initial, gen[, j], kill[, j])
  }

  initial
}


#' Compute Gen and Kill Sets for Data Flow Analysis
#'
#' This function helps with computing the gen and kill sets for a data flow
#' analysis.
#'
#' @param cfg (BlockList) A control flow graph on which to compute gen and kill
#' sets.
#' @param fun (function) A function which accepts an `ASTNode` and returns a
#' list with elements named `gen` and `kill`. Each element should be a logical
#' vector with the same length as `universe`.
#' @param universe (named list) The universal set, which contains all items
#' used in the analysis.
#' @param ... Additional arguments to `fun`.
#'
#' @seealso [forward_analysis()]
#' @export
compute_gen_kill =
function(cfg, fun, universe, ...)
{
  n_items = length(universe)

  n_blocks = length(cfg$contents)
  gen = matrix(NA, n_items, n_blocks)
  kill = matrix(NA, n_items, n_blocks)
  rownames(gen) = names(universe)
  rownames(kill) = names(universe)

  # Compute kill and gen sets for each block.
  for (b in seq_len(n_blocks)) {
    block = cfg[[b]]

    # Compute kill and gen sets for each line.
    n_lines = length(block)
    b_kill = matrix(NA, n_items, n_lines)
    b_gen = matrix(NA, n_items, n_lines)

    for (j in seq_len(n_lines)) {
      line = block[[j]]
      kg = fun(line, universe, ...)

      b_gen[, j] = kg[["gen"]]
      b_kill[, j] = kg[["kill"]]
    }

    # Aggregate line-level sets to block-level.
    gen[, b] = block_aggregate(b_gen, b_kill, forward = FALSE)
    kill[, b] = row_ors(b_kill)
  }

  list(gen = gen, kill = kill)
}
