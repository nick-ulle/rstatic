
#' @export
FlowGraph = R6::R6Class("FlowGraph",
  "public" = list(
    next_id = 1L,
    blocks = list(),
    graph = NULL,

    initialize = function() {
      self$graph = igraph::make_empty_graph()
    },

    add_vertex = function(id) {
      if (missing(id)) {
        id = sprintf("%%%i", self$next_id)
        self$next_id = self$next_id + 1L
      }

      self$graph = self$graph + igraph::vertex(id)

      return (id)
    },

    add_edge = function(from, to) {
      self$graph = self$graph + igraph::edge(from, to)

      invisible (NULL)
    },

    get_index = function(name) {
      match(name, igraph::V(self$graph)$name)
    },

    get_name = function(index) {
      igraph::V(self$graph)$name[index]
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


#' @export
names.FlowGraph = function(x) {
  names(x$blocks)
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


#' @export
ControlFlowGraph = R6::R6Class("ControlFlowGraph", inherit = FlowGraph,
  "public" = list(
    params = list(),
    entry = NULL,
    exit = NULL,

    initialize = function() {
      super$initialize()

      self$entry = self$add_vertex()
      self$blocks[[self$entry]] = BasicBlock$new()

      self$exit = self$add_vertex()
      exit_block = BasicBlock$new()
      exit_block$terminator = RetTerminator$new()
      self$blocks[[self$exit]] = exit_block

      return (self)
    },

    # FIXME: Make sure copying works correctly.
    set_params = function(value) {
      for (v in value)
        v$parent = self

      self$params = value
    }
  )
)
