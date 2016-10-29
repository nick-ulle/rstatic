
# Classes that represent basic blocks.

#' @include stack.R
NULL

#' @export
CFGBuilder = R6::R6Class("CFGBuilder",
  public = list(
    block = NULL,
    loop_entry = NULL,
    loop_exit = NULL,

    initialize = function(block = BasicBlock$new()) {
      self$block = block
      self$loop_entry = Stack$new()
      self$loop_exit = Stack$new()
    }
  )
)


#' @export
BasicBlock = R6::R6Class("BasicBlock",
  public = list(
    name = ""
    , body = NULL
    , terminator = NULL
    , predecessors = list()

    , initialize = function(body = list()) {
      self$body = body
      return (self)
    }

    , add_predecessor = function(block) {
      # FIXME: use a union here.
      self$predecessors = append(self$predecessors, block)
      return (self)
    }

    , set_branch = function(block_true, block_false = NULL, predicate = NULL) {
      self$terminator = BranchInst$new(block_true, block_false, predicate)
      block_true$add_predecessor(self)
      if (!is.null(block_false))
        block_false$add_predecessor(self)

      return (self)
    }

    , set_iterate = function(block_body, block_exit, ivar, iter) {
      self$terminator = IterateInst$new(block_body, block_exit, ivar, iter)
      block_body$add_predecessor(self)
      block_exit$add_predecessor(self)

      return (self)
    }
  ),

  active = list(
    is_terminated = function() {
      return (!is.null(self$terminator))
    }

    , successors = function() {
      if (self$is_terminated)
        return (self$terminator$successors)
      return (list())
    }
  )
)


Terminator = R6::R6Class("Terminator")

#' @export
BranchInst = R6::R6Class("BranchInst", inherit = Terminator,
  public = list(
    predicate = NULL
    , block_true = NULL
    , block_false = NULL

    , initialize = function(block_true, block_false = NULL, predicate = NULL) {
      self$block_true = block_true
      self$block_false = block_false
      self$predicate = predicate
    }
  ),

  active = list(
    successors = function() {
      return (list(self$block_true, self$block_false))
    }
  )
)

#' @export
IterateInst = R6::R6Class("IterateInst", inherit = Terminator,
  public = list(
    block_body = NULL
    , block_exit = NULL
    , ivar = NULL
    , iter = NULL

    , initialize = function(block_body, block_exit, ivar, iter) {
      self$block_body = block_body
      self$block_exit = block_exit
      self$ivar = ivar
      self$iter = iter
    }
  ),

  active = list(
    successors = function() {
      return (list(self$block_body, self$block_exit))
    }
  )
)
