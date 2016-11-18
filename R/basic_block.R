# Classes that represent basic blocks.


#' @export
BasicBlock = R6::R6Class("BasicBlock",
  public = list(
    body = NULL,
    terminator = NULL,
    predecessors = integer(0),

    initialize = function(body = list()) {
      self$body = body
      return (self)
    },

    add_predecessor = function(block) {
      self$predecessors = union(self$predecessors, block)
      return (self)
    },

    append = function(node) {
      self$body = append(self$body, node)
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
      return (integer(0))
    }
  )
)


Terminator = R6::R6Class("Terminator")

#' @export
BranchInst = R6::R6Class("BranchInst", inherit = Terminator,
  public = list(
    true = NULL, # FIXME:
    false = NULL,
    condition = NULL,

    initialize = function(to_t, to_f = NULL, condition = NULL) {
      self$true = to_t
      self$false = to_f
      self$condition = condition
    }
  ),

  active = list(
    successors = function() {
      return (c(self$true, self$false))
    }
  )
)

#' @export
IterateInst = R6::R6Class("IterateInst", inherit = Terminator,
  public = list(
    to = integer(2), # FIXME:
    ivar = NULL,
    iter = NULL,

    initialize = function(to_body, to_exit, ivar, iter) {
      self$to[[1]] = to_body
      self$to[[2]] = to_exit
      self$ivar = ivar
      self$iter = iter
    }
  ),

  active = list(
    successors = function() {
      return (self$to)
    }
  )
)
