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

  list(entry = live[["update"]], exit = live[["result"]], killgen = killgen)
}


#' Compute Kill and Gen Sets for Live Variables Analysis
#'
#' Given a basic block, this function computes the kill and gen sets for live
#' variables analysis.
#'
#' @param node (ASTNode) The basic block to compute
#' @param result
#'
live_variables_killgen = function(node, killgen) {
  UseMethod("live_variables_killgen")
}

#' @export
live_variables_killgen.Block = function(node, killgen) {
  # Kills only happen in LHS of assignment. Also `rm()`
  # Uses can be widespread.
  for (line in node$body) {
    killgen = live_variables_killgen(line, killgen)
  }

  killgen
}

#' @export
live_variables_killgen.Assign = function(node, killgen) {
  killgen = live_variables_killgen(node$read, killgen)

  # Add LHS to killed.
  killgen[["kill"]] = union(killgen[["kill"]], node$write$name)

  killgen
}

# live_variables_killgen.Phi

#' @export
live_variables_killgen.Application = function(node, killgen) {
  for (arg in node$args) {
    killgen = live_variables_killgen(arg, killgen)
  }

  killgen
}

#' @export
live_variables_killgen.Call = function(node, killgen) {
  killgen = NextMethod()
  live_variables_killgen(node$fn, killgen)
}

#' @export
live_variables_killgen.If = function(node, killgen) {
  live_variables_killgen(node$condition, killgen)
}

#' @export
live_variables_killgen.Symbol = function(node, killgen) {
  used = node$name

  # Check whether this symbol has already been killed in this block.
  if (used %in% killgen[["kill"]])
    return (killgen)

  killgen[["gen"]] = union(killgen[["gen"]], used)
  killgen
}

# live_variables_killgen.Parameter

#' @export
live_variables_killgen.Literal = function(node, killgen) killgen

# Loops have an If generated in the CFG, so no need to do anything here.
#' @export
live_variables_killgen.Loop = function(node, killgen) killgen

#' @export
live_variables_killgen.NULL = function(node, killgen) killgen

#' @export
live_variables_killgen.Function = function(node, killgen) killgen
