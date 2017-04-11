
#' @export
FlowGraph = R6::R6Class("FlowGraph",
  "public" = list(
    next_id = 1L,
    blocks = list(),
    graph = NULL,
    entry = NULL,
    exit = NULL,

    initialize = function() {
      self$graph = igraph::make_empty_graph()
    },

    add_vertex = function() {
      id = as.character(self$next_id)
      self$next_id = self$next_id + 1L

      self$graph = self$graph + igraph::vertex(id)

      return (id)
    },

    add_edge = function(from, to) {
      self$graph = self$graph + igraph::edge(from, to)

      invisible (NULL)
    }
  )
)


#' @export
`[[.FlowGraph` = function(x, i) {
  x$blocks[[i]]
}


#' @export
`[[<-.FlowGraph` = function(x, i, value) {
  x$blocks[[i]] = value
  return (x)
}


#' @export
length.FlowGraph = function(x) {
  length(x$blocks)
}

#' Plot Method for Flow Graphs
#'
#' This method plots a flow graph.
#'
#' @param x (FlowGraph) A flow graph.
#' @param ... Additional arguments to \code{plot.igraph}.
#'
#' @export
plot.FlowGraph = function(x, ...) {
  plot(x$graph, ...)
}
