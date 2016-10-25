
# Classes that represent basic blocks.

#' @include stack.R
NULL

#' @export
CFGState = R6::R6Class("CFGState",
  public = list(
    breaks = Stack$new(),
    nexts = Stack$new()
  )
)

#' @export
BasicBlock = R6::R6Class("BasicBlock",
  public = list(
    name = "",
    predicate = NULL,
    predecessors = list(),
    successors = list(),
    body = NULL,

    initialize = function(body = list()) {
      self$body = body
      return (self)
    },

    append_node = function(node) {
      self$body = append(self$body, node)
      return (self)
    },

    add_predecessor = function(block) {
      self$predecessors = append(self$predecessors, block)
      return (self)
    },

    set_jump = function(block) {
      self$successors = list(block)
      block$add_predecessor(self)

      return (self)
    },

    set_branch = function(predicate, true, false) {
      self$predicate = predicate

      self$successors = list(true, false)
      true$add_predecessor(self)
      false$add_predecessor(self)

      return (self)
    },

    set_iterator = function(ivar, iter, body) {
    }
  )
)
