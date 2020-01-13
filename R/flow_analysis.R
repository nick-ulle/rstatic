#' Solve a Data Flow Analysis
#'
#' This function finds a solution to a data flow analysis.
#'
#' This function uses the iterative maximal fixed point algorithm to find a
#' solution. The solution is not necessarily unique.
#'
#' @param cfg (BlockList) A control flow graph with which to compute a
#' solution.
#' @param initial (logical matrix) Initial guess of solution for each block.
#' @param gk_list (list of GenKillSets) A list of the gen and kill sets for
#' each block.
#' @param forward (logical) Is this a forward data flow analysis? If `FALSE`,
#' the solver will traverse the control flow graph backward.
#' @param confluence (function) A function to combine sets at a merge in
#' control flow. Defaults to `|`, which unions the sets.
#' @param update (function) A function to update a block's set based on the gen
#' and kill sets. Defaults to `dfa_standard_update`.
#' @param ... Additional arguments to `update`.
#' @param aggregate (logical) Aggregate results to the block-level?
#' @param max_iter (integer) Maximum number of iterations to run.
#'
#' @return If `aggregate = FALSE` (the default), a list of logical matrices.
#' Each list element corresponds to one block in `cfg`. For each matrix, rows
#' correspond to set items. Columns represent entry sets for lines in the
#' block; the last column is the exit set for the last line (equivalently, for
#' the block).
#'
#' If `aggregate = TRUE`, a two-element list. The first element, "entry", is a
#' logical matrix that represents the solution sets at the entry of each block.
#' The second element, "exit", is a logical matrix that represents the solution
#' sets at the exit of each block. For both matrices, rows correspond to set
#' items and columns correspond to blocks.
#'
#' @seealso [dfa_initialize()], [dfa_gen_kill()], [dfa_standard_update()]
#' @export
dfa_solve =
function(cfg, initial, gk_list
  , forward = TRUE
  , confluence = `|`
  , update = dfa_standard_update
  , ...
  , aggregate = FALSE, max_iter = 1000L)
{
  if (is(cfg, "BlockList"))
    cfg = compute_cfg(cfg)

  gk = dfa_aggregate(gk_list, forward = forward)
  gen = gk@gen
  kill = gk@kill

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

  # <<<--- MFP solver -------------------------
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
  # ---------------------------------------->>>

  if (aggregate) {
    # The exit sets can be computed by an update() on each entry set.
    updated = update(initial, gen, kill, ...)

    if (forward)
      return (list(entry = initial, exit = updated))
    else
      return (list(entry = updated, exit = initial))
  }

  dfa_disaggregate(initial, gk_list, forward = forward, update = update, ...)
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


#' Aggregate Data Flow Analysis Gen and Kill Sets to Block-level
#'
#' This function aggregates line-level gen and kill sets for a data flow
#' analysis to block-level by propagating them through the block.
#'
#' @param gen (logical matrix) Gen sets for each line.
#' @param kill (logical matrix) Kill sets for each line.
#' @param forward (logical) Should sets be aggregated forward (first to last
#' line) or backward (last to first line)?
#' @param ... Additional arguments to [dfa_propagate()].
#'
#' @return A logical vector with `nrow(gen)` elements. The set computed by
#' aggregating over all of the line-level sets.
#'
#' @seealso [dfa_propagate()], [dfa_standard_update()]
#' @export
dfa_aggregate =
function(gk_list, forward = TRUE, ...)
{
  gen1 = gk_list[[1L]]@gen
  n_items = nrow(gen1)
  n_blocks = length(gk_list)

  gen = matrix(NA, n_items, n_blocks)
  kill = matrix(NA, n_items, n_blocks)
  rownames(gen) = rownames(gen1)
  rownames(kill) = rownames(gen1)

  # Aggregate each block.
  for (b in seq_len(n_blocks)) {
    gk = gk_list[[b]]

    sets = dfa_propagate(initial = FALSE, gk = gk, forward = forward, ...)
    if (forward)
      gen[, b] = sets[, ncol(sets)]
    else
      gen[, b] = sets[, 1L]

    kill[, b] = row_ors(gk@kill)
  }

  GenKillSets(gen, kill)
}


#' @export
dfa_disaggregate =
function(sets, gk_list, ...)
# Propagate results within each block to get back to line-level information.
{
  n_blocks = ncol(sets)
  result = vector("list", n_blocks)
  for (j in seq_len(n_blocks))
    result[[j]] = dfa_propagate(sets[, j], gk_list[[j]], ...)

  result
}

#' Propagate Data Flow Analysis Result Sets within a Block
#'
#' This function propagates result sets from a data flow analysis at line-level
#' by sequential application of the update function.
#'
#' @param initial (logical matrix) Initial set to propagate. For forward
#' propagation, this is the entry set for the block. For backward propagation,
#' this is the exit set for the block.
#' @param gen (logical matrix) Gen sets for each line.
#' @param kill (logical matrix) Kill sets for each line.
#' @param forward (logical) Should sets be aggregated forward (first to last
#' line) or backward (last to first line)?
#' @param update (function) A function to compute the update set for a block.
#'
#' @return A logical matrix with `nrow(gen)` rows and `ncol(gen) + 1` columns.
#' Rows correspond to set items. Columns represent entry sets for lines; the
#' last column is the exit set for the last line (equivalently, for the block).
#'
#' @seealso [dfa_aggregate()], [dfa_disaggregate()], [dfa_standard_update()]
#' @export
dfa_propagate =
function(initial
  , gk
  , forward = TRUE
  , update = dfa_standard_update
  , ...)
{
  gen = gk@gen
  kill = gk@kill

  # Add column for initial sets.
  n = ncol(gen)
  sets = matrix(NA, nrow(gen), n + 1L)
  rownames(sets) = rownames(gen)

  if (forward) {
    sets[, 1L] = initial
    index = seq(1L, n)

    for (j in index)
      # Here:
      #   sets[, j + 1L] is exit set for line j
      #    gen[, j     ] is gen set for line j
      sets[, j + 1L] = update(sets[, j], gen[, j], kill[, j], ...)

  } else {
    sets[, n + 1L] = initial
    index = seq(n + 1L, 2L)

    for (j in index)
      # Here:
      #   sets[, j - 1L] is entry set for line j - 1L
      #    gen[, j - 1L] is gen set for line j - 1L
      sets[, j - 1L] = update(sets[, j], gen[, j - 1L], kill[, j - 1L], ...)
  }

  sets
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
function(x, fun, universe, ...)
{
  UseMethod("dfa_gen_kill")
}


# FIXME: Document methods separately?
#' @export
dfa_gen_kill.BlockList =
function(x, fun, universe, ...)
{
  lapply(x$contents, dfa_gen_kill, fun, universe, ...)
}


#' @export
dfa_gen_kill.Block =
function(x, fun, universe, ...)
  # Compute line-level gen and kill sets for a single block.
{
  n_items = length(universe)

  # Compute gen and kill sets for each line.
  n_lines = length(x)
  gen = matrix(NA, n_items, n_lines)
  kill = matrix(NA, n_items, n_lines)
  rownames(gen) = names(universe)
  rownames(kill) = names(universe)

  for (j in seq_len(n_lines)) {
    line = x[[j]]
    gk = fun(line, universe, ...)

    gen[, j] = gk[["gen"]]
    kill[, j] = gk[["kill"]]
  }

  GenKillSets(gen, kill)
}


setClass("rstatic::GenKillSets",
  slots = list(
    gen = "matrix",
    kill = "matrix"
  ))

GenKillSets =
function(gen, kill)
{
  new("rstatic::GenKillSets", gen = gen, kill = kill)
}
