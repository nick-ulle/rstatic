#' Live Variables Analysis
#'
#' This function computes the live variables at the exit of each basic block. A
#' variable is live if it has already been defined and it will be used in some
#' subsequent block.
#'
#' @param cfg (ControlFlowGraph) The control flow graph to compute the live
#' variables analysis for.
#' @param ... Additional arguments to \code{backward_analysis()}.
#'
#' @export
live_variables = function(cfg, ...) {
  # Set up empty sets for each block.
  initial = replicate(length(cfg), character(0), simplify = FALSE)
  names(initial) = names(cfg$blocks)

  # Compute kill and gen sets for each block.
  killgen = list(kill = character(0), gen = character(0))
  killgen = lapply(cfg$blocks, live_variables_killgen, killgen)

  # Solve the analysis equations.
  live = backward_analysis(cfg, initial, killgen, ...)

  # >>>>>
  kills = lapply(killgen, function(x) x$kill)
  gens = lapply(killgen, function(x) x$gen)

  if (length(live[["update"]]) == 0)
    live[["update"]] = replicate(4, character(0))

  data.frame(
    kill = I(kills)
    , gen = I(gens)
    , entry = I(live[["update"]])
    , exit = I(live[["result"]])
  )
  #list(entry = live[["update"]], exit = live[["result"]], killgen = killgen)
}


#' Compute Kill and Gen Sets for Live Variables Analysis
#'
#' Given a basic block, this function computes the kill and gen sets for live
#' variables analysis.
#'
#' @param node (ASTNode) The basic block to compute
#' @param initial Initial
#' @param result
#'
live_variables_killgen = function(node, initial) {
  UseMethod("live_variables_killgen")
}

#' @export
live_variables_killgen.Assign = function(node, initial) {
  initial[["kill"]] = union(initial[["kill"]], node$write$name)

  initial = live_variables_killgen(node$read, initial)
}

#' @export
live_variables_killgen.Return = live_variables_killgen.Assign

# live_variables_killgen.Phi

#' @export
live_variables_killgen.Application = function(node, initial) {
  for (arg in node$args) {
    initial = live_variables_killgen(arg, initial)
  }

  initial
}

#' @export
live_variables_killgen.Call = function(node, initial) {
  live_variables_killgen(node$fn, NextMethod())
}

#' @export
live_variables_killgen.If = function(node, initial) {
  live_variables_killgen(node$condition, initial)
}

#' @export
live_variables_killgen.While = live_variables_killgen.If

#' @export
live_variables_killgen.For = function(node, initial) {
  initial[["kill"]] = union(initial[["kill"]], node$variable$name)

  live_variables_killgen(node$iterator, initial)
}

#' @export
live_variables_killgen.Symbol = function(node, initial) {
  initial[["gen"]] = union(initial[["gen"]], node$name)

  initial
}

# live_variables_killgen.Parameter

#' @export
live_variables_killgen.Literal = function(node, initial) initial

#' @export
live_variables_killgen.Branch = live_variables_killgen.Literal

#' @export
live_variables_killgen.Function = live_variables_killgen.Literal
