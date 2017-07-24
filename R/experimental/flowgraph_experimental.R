# Revised data structure for control-flow graphs.
#
# TODO: igraph seems like more trouble than it's worth because:
#
#   * Gabor does not recommend it for graphs that will change a lot.
#   * Vertex and edge IDs are not stable across changes to the graph structure.

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


`[[.FlowGraph` = function(x, i) {
  x$blocks[[i]]
}

`[[<-.FlowGraph` = function(x, i, value) {
  x$blocks[[i]] = value
  return (x)
}


CFGBuilder = R6::R6Class("CFGBuilder",
  "public" = list(
    cfg = NULL,
    contexts = NULL,
    cursor = NULL,

    initialize = function(cfg = FlowGraph$new()) {
      self$cfg = cfg
      self$contexts = Stack$new()

      entry = self$cfg$add_vertex()
      self$cfg$entry = entry
      self$cfg[[entry]] = FnEntryBlock$new()

      exit = self$cfg$add_vertex()
      self$cfg$exit = exit
      self$cfg[[exit]] = FnExitBlock$new()

      self$push_context(entry, exit)
      self$cursor = entry
    },

    new_block = function() {
      id = self$cfg$add_vertex()
      self$cfg[[id]] = BasicBlock$new()

      return (id)
    },

    branch_to = function(to, from = self$cursor) {
      self$cfg$add_edge(from, to)
      self$cursor = to

      self$cfg[[from]]$set_branch(to)

      invisible (NULL)
    },

    branch_to_if = function(condition, to_t, to_f, from = self$cursor) {
      self$cfg$add_edge(from, to_t)
      self$cfg$add_edge(from, to_f)
      self$cursor = to_t

      self$cfg[[from]]$set_branch(condition)

      invisible (NULL)
    },

    branch_to_for = function() {
      self$cfg[[from]]$set_branch()
    }

    add_next = function(from = self$cursor) {
      to = self$contexts$peek()[[1]]
      self$add_branch(to, from)

      invisible (NULL)
    },

    add_break = function(from = self$cursor) {
      to = self$contexts$peek()[[2]]
      self$add_branch(to, from)

      invisible (NULL)
    },

    add_return = function(from = self$cursor) {
      # FIXME:
      to = self$contexts
      self$add_branch(to, from)

      invisible (NULL)
    },

    #close_branch = function() {
    #  if (self$branch_open)
    #    self$cfg$branch(
    #},

    push_context = function(entry, exit) {
      self$contexts$push(c(entry, exit))
      invisible (NULL)
    },

    pop_context = function() {
      self$contexts$pop()
    }
  )
)
