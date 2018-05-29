#' Live Variables Analysis
#'
#' Given code in basic blocks, this function computes the live variables at the
#' exit (and optionally, the entry) of each block.
#'
#' A variable is live if it has already been defined and it will be used in
#' some subsequent block.
#'
#' @param node The Function or BlockList to analyze.
#' @param cfg (igraph) The control flow graph for the given code.
#' @param du (list) Definition and use sets for each block, as computed by
#' \code{def_use_sets()}.
#' @param ... Additional arguments to \code{backward_analysis()}.
#'
#' @return The solution returned by \code{backward_analysis()}.

#'
#' @export
live_variables =
function(node
  , cfg = compute_cfg(node)
  # Kill and gen sets for each block.
  , du = def_use_sets(node, by_block = TRUE, only_undefined_uses = TRUE)
  , ...)
{
  if (is(node, "Function"))
    node = node$body

  len = length(node$contents)
  initial = replicate(len, character(0), simplify = FALSE)

  backward_analysis(cfg, initial, du[["use"]], du[["def"]], ...)
}
