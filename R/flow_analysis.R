#' Solve a Data Flow Analysis
#'
#' This function finds a solution to a data flow analysis.
#'
#' This function uses the iterative maximal fixed point algorithm to find a
#' solution. The solution is not necessarily unique.
#'
#' @param cfg (BlockList) A control flow graph with which to compute a
#' solution.
#' @param initial Initial guess of solution for each block.
#' @param gen (logical matrix) The gen sets as a logical matrix. Rows
#' correspond to items and columns correspond to blocks.
#' @param kill (logical matrix) The kill sets as a logical matrix. Rows
#' correspond to items and columns correspond to blocks.
#' @param forward (logical) Is this a forward data flow analysis? If `FALSE`,
#' the solver will traverse the control flow graph backward.
#' @param confluence (function) A function to combine sets at a merge in
#' control flow. Defaults to `|`, which unions the sets.
#' @param ... Additional arguments to `update`.
#' @param update (function) A function to update a block's set based on the gen
#' and kill sets. Defaults to `dfa_standard_update`.
#' @param max_iter (integer) Maximum number of iterations to run.
#' @param full_analysis (logical) If `FALSE`, either the entry sets or exit
#' sets are computed, but not both. Entry sets are always computed for a
#' forward analysis, and exit sets are always computed for a backward analysis.
#'
#' @return A two-element list. The first element, "entry", is a logical matrix
#' that represents the solution sets at the entry of each block. The second
#' element, "exit", is a logical matrix that represents the solution sets at
#' the exit of each block. For both matrices, rows correspond to set items and
#' columns correspond to blocks.
#'
#' @seealso [dfa_gen_kill()], [dfa_standard_update()]
#' @export
dfa_solve =
function(cfg, initial, gen, kill
  , forward = TRUE
  , confluence = `|`
  , ...
  , update = dfa_standard_update
  , full_analysis = TRUE, max_iter = 1000L)
{
  if (is(cfg, "BlockList"))
    cfg = compute_cfg(cfg)

  if (forward) {
    # For forward analysis, source vertex comes first.
    edge_src = 1L
    edge_dst = 2L
    edge_mode = "out"
  } else {
    # For backward analysis, destination vertex comes first.
    edge_src = 2L
    edge_dst = 1L
    edge_mode = "in"
  }

  # Step 1: Initialize worklist of edges.
  worklist = igraph::ends(cfg, igraph::E(cfg))

  # Step 2: Iterate until worklist is empty.
  iter = 0L
  while (nrow(worklist) > 0L && iter < max_iter) {
    b      = worklist[1L, edge_src]
    b_next = worklist[1L, edge_dst]
    worklist = worklist[-1L, , drop = FALSE]

    # Update result set for the next block.
    old = initial[, b_next]
    update_set = update(initial[, b], gen[, b], kill[, b], ...)
    new = confluence(old, update_set)

    # Check whether this iteration changed anything.
    if (sum(old) != sum(new)) {
      initial[, b_next] = new

      # Add edges from b_next to successors.
      next_edges = igraph::incident(cfg, b_next, edge_mode)
      to_bind = igraph::ends(cfg, next_edges)
      worklist = rbind(to_bind, worklist)
    }

    iter = iter + 1L
  }

  # Step 3: The exit sets can be computed by an update() on each entry set.
  exit = if (full_analysis) {
    update(initial, gen, kill)
  } # else NULL

  if (forward)
    list(entry = initial, exit = exit)
  else
    list(entry = exit, exit = initial)
}


#' Initial Sets for Data Flow Analysis
#'
#' This function generates initial sets for a data flow analysis.
#'
#' @param cfg (BlockList) A control flow graph for which to initialize the
#' sets.
#' @param universe (named list) The universal set, which contains all items
#' used in the analysis.
#' @param default (logical) If `TRUE`, sets are initialized to the universal
#' set. Otherwise, sets are initialized to the empty set.
#' @param complement (character) A block which should be initialized to the
#' complement of the default set.
#'
#' @return A logical matrix that represents the solution set of each block.
#' Rows correspond to set items and columns correspond to blocks.
#'
#' @seealso [dfa_solve()]
#' @export
dfa_initialize =
function(cfg, universe, default = FALSE
  , complement = c("none", "entry", "exit"))
{
  complement = match.arg(complement)

  initial = matrix(default, length(universe), length(cfg))
  rownames(initial) = names(universe)

  idx = switch(complement
    , none = return(initial)
    , entry = cfg$entry_index
    , exit = cfg$exit_index
  )
  initial[, idx] = !initial[, idx]
  initial
}


#' Standard Update for Data Flow Analysis
#'
#' This function computes the standard update set for a data flow analysis. The
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
#' @seealso [dfa_solve()], [dfa_aggregate()]
#' @export
dfa_standard_update =
function(result, gen, kill, ...)
{
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
#' @seealso [dfa_standard_update()]
#' @export
dfa_aggregate =
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
#' @return A two-element list. The first element, "gen", is a logical matrix
#' that represents the gen set for each block. The second element, "kill", is
#' a logical matrix that represents the kill set for each block. For both
#' matrices, rows correspond to set items and columns correspond to blocks.
#'
#' @seealso [dfa_solve()]
#' @export
dfa_gen_kill =
function(cfg, fun, universe, forward = TRUE, ...)
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
    gen[, b] = dfa_aggregate(b_gen, b_kill, forward = forward)
    kill[, b] = row_ors(b_kill)
  }

  list(gen = gen, kill = kill)
}
