
#' @include stack.R
NULL


#' @export
CFGraph = R6::R6Class("CFGraph",
  public = list(
    blocks = list(),
    entry = NA_integer_,
    exit = NA_integer_,
    len = 0L,
    loop_open = TRUE,

    new_block = function() {
      self$len = self$len + 1L
      self$blocks[[self$len]] = BasicBlock$new()

      return (self$len)
    },

    jump = function(to, from = self$exit) {
      self$blocks[[from]]$terminator = BranchInst$new(to)
      self$blocks[[to]]$add_predecessor(from)
      self$exit = to
      return (self)
    },

    branch = function(to_t, to_f, condition, from = self$exit) {
      self$blocks[[from]]$terminator = BranchInst$new(to_t, to_f, condition)
      self$blocks[[to_t]]$add_predecessor(from)
      self$blocks[[to_f]]$add_predecessor(from)
      self$exit = to_t
      return (self)
    },

    iterate = function(to_t, to_f, ivar, iter, from = self$exit) {
      self$blocks[[from]]$terminator = IterateInst$new(to_t, to_f, ivar, iter)
      self$blocks[[to_t]]$add_predecessor(from)
      self$blocks[[to_f]]$add_predecessor(from)
      self$exit = to_t
      return (self)
    },

    change_branch = function(id) {
      self$exit = id
      self$loop_open = TRUE
      return (self)
    },

    loop_push = function(entry, exit) {
      private$loop_stack$push(c(entry, exit))
      return (self)
    },

    loop_pop = function() {
      return (private$loop_stack$pop())
    },

    loop_break = function(from = self$exit) {
      exit = private$loop_stack$peek()[[2]]
      self$jump(from, exit)
      self$loop_open = FALSE
      return (self)
    },

    loop_next = function(from = self$exit) {
      entry = private$loop_stack$peek()[[1]]
      self$jump(from, entry)
      self$loop_open = FALSE
      return (self)
    },

    initialize = function() {
      self$entry = self$exit = self$new_block()
      private$loop_stack = Stack$new()

      return (self)
    }
  ),
  active = list(
    entry_block = function() {
      return (self$blocks[[self$entry]])
    },

    exit_block = function() {
      return (self$blocks[[self$exit]])
    }
  ),
  private = list(
    loop_stack = NULL
  )
)
