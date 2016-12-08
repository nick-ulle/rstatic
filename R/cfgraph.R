# Data structure for control-flow graphs.

#' @include stack.R
#' @include basic_block.R
NULL


#' @export
CFGraph = R6::R6Class("CFGraph",
  public = list(
    blocks = list(),
    entry = NA_integer_,
    exit = NA_integer_,
    exit_fn = NA_integer_,
    len = 0L,
    branch_open = TRUE,

    get_postorder = function(from = self$entry) {
      # Compute the postorder traversal.
      to_visit = Stack$new(type = "integer")
      to_visit$push(from)

      is_discovered = logical(self$len)
      visited = integer(self$len)
      n_visited = 0

      while (!to_visit$is_empty) {
        idx = to_visit$peek()
        is_discovered[[idx]] = TRUE

        succ = self$blocks[[idx]]$successors
        succ = succ[!is_discovered[succ]]

        if (length(succ) > 0) {
          # Undiscovered successors, so push them onto stack.
          to_visit$push_many(succ)
        } else {
          # No undiscovered successors, so visit and pop this node.
          to_visit$pop()
          n_visited = n_visited + 1
          visited[n_visited] = idx
        } # if

      } # while

      return (visited)
    },

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
      self$branch_open = TRUE
      return (self)
    },

    fn_return = function(from = self$exit) {
      if (is.na(self$exit_fn))
        stop("no exit block to return to.",
          "\n  Did you attempt to use return() outside of a function?")

      self$jump(from = from, self$exit_fn)
      self$branch_open = FALSE
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
      self$jump(from = from, exit)
      self$branch_open = FALSE
      return (self)
    },

    loop_next = function(from = self$exit) {
      entry = private$loop_stack$peek()[[1]]
      self$jump(from = from, entry)
      self$branch_open = FALSE
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


#' @export
`[[.CFGraph` = function(x, i) {
  x$blocks[[i]]
}


#' @export
length.CFGraph = function(x) {
  x$len
}
