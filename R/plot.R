
#' Plot Method for Control-flow Graphs
#'
#' This method plots a control flow graph using the igraph package.
#'
#' @param x (CFGraph) A control-flow graph.
#' @param ... Additional arguments to \code{plot.igraph}.
#'
#' @export
plot.CFGraph = function(x, ...) {
  plot(as_igraph(x), ...)
}
