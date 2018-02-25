
#' @export
FlowGraph = R6::R6Class("FlowGraph",
  "private" = list(
    deep_clone = function(name, value) {
      switch(name,
        "blocks" = lapply(value, function(v) v$copy()),
        if (is(value, "R6")) value$clone(deep = TRUE)
        else value
      )
    }
  ),

  "public" = list(
    next_id = 1L,
    blocks = list(),
    graph = NULL,

    initialize = function() {
      self$graph = igraph::make_empty_graph()
    },

    copy = function() self$clone(deep = TRUE),

    add_vertex = function(id = NULL) {
      if (is.null(id)) {
        id = sprintf("%%%i", self$next_id)
        self$next_id = self$next_id + 1L
      }

      self$graph = self$graph + igraph::vertex(id)

      return (id)
    },

    remove_vertex = function(id) {
      self$graph = self$graph - igraph::vertex(id)
      invisible(NULL)
    },

    add_edge = function(from, to) {
      self$graph = self$graph + igraph::edge(from, to)

      invisible(NULL)
    },

    get_index = function(name) {
      match(name, igraph::V(self$graph)$name)
    },

    get_name = function(index) {
      igraph::V(self$graph)$name[index]
    },

    reorder = function(ordering) {
      self$blocks = self$blocks[ordering, drop = FALSE]

      # Transform the ordering into a permutation.
      permutation = seq_along(ordering)
      permutation[ordering] = permutation
      self$graph = permute.vertices(self$graph, permutation)

      invisible(NULL)
    }
  )
)

#' @export
`[.FlowGraph` = function(x, i) {
  # NOTE: The coercion for igraph vertices that used to be here was not needed.
  # In a sane FlowGraph object, the order of the vertices will always match the
  # order of the list.
  x$blocks[i]
}

#' @export
`[[.FlowGraph` = function(x, i) {
  x$blocks[[i]]
}


#' @export
`[[<-.FlowGraph` = function(x, i, value) {
  x$blocks[[i]] = value
  x
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
    entry = NULL,
    exit = NULL,

    initialize = function(
      fn = NULL,
      exit_block = Block$new(Symbol$new("._return_"))
    ) {
      super$initialize()

      #self$entry = self$add_vertex("entry")
      self$exit = self$add_vertex("%exit")

      exit_block$parent = fn
      self$blocks[[self$exit]] = exit_block

    },

    add_block = function(block = Block$new(), id = NULL) {
      if (is.null(self$entry)) {
        if (is.null(id)) {
          id = "%entry"
          self$entry = id
        } else {
          self$entry = id
        }
      }

      id = self$add_vertex(id)
      self$blocks[[id]] = block

      id
    }
  )
)


#' @export
DataFlowGraph = R6::R6Class("DataFlowGraph", inherit = FlowGraph,
  "public" = list(
    global_uses = character(0),

    add_use = function(node, id = NULL) {
      id = self$add_vertex(id)
      self$graph = set_vertex_attr(self$graph, "is_def", id, FALSE)
      self$graph = set_vertex_attr(self$graph, "basename", id, NA)
      self$blocks[[id]] = node

      id
    },

    add_def = function(node, basename, id = NULL) {
      id = self$add_vertex(id)
      self$graph = set_vertex_attr(self$graph, "is_def", id, TRUE)
      self$graph = set_vertex_attr(self$graph, "basename", id, basename)
      self$blocks[[id]] = node

      id
    },

    add_edge = function(def, use) {
      if (def %in% names(self$blocks))
        super$add_edge(def, use)
      else
        self$global_uses = union(self$global_uses, def)

      invisible(NULL)
    }
  )
)
