# FIXME: Copying Terminators doesn't handle circular references correctly.

Terminator = R6::R6Class("Terminator", inherit = ASTNode,
  "public" = list(
    successors = function() integer(0)
  )
)

#' @export
RetTerminator = R6::R6Class("RetTerminator", inherit = Terminator,
  "public" = list(
    value = NULL,

    initialize = function(value = Symbol$new("._return_")) {
      self$set_value(value)
    },

    set_value = function(value) {
      value$parent = self
      self$value = value
    }
  )
)

#' @export
BrTerminator = R6::R6Class("BrTerminator", inherit = Terminator,
  "public" = list(
    dest = NULL,

    initialize = function(dest) {
      self$dest = dest
    }
  )
)

#' @export
CondBrTerminator = R6::R6Class("CondBrTerminator", inherit = Terminator,
  "public" = list(
    true = NULL,
    false = NULL,
    condition = NULL,

    initialize = function(true, false, condition) {
      self$true = true
      self$false = false
      self$set_condition(condition)
    },

    successors = function() {
      c(self$true, self$false)
    },

    set_condition = function(value) {
      if (!is.null(value))
        value$parent = self

      self$condition = value
    }
  )
)

#' @export
IterTerminator = R6::R6Class("IterTerminator", inherit = CondBrTerminator,
  public = list(
    ivar = NULL,
    iter = NULL,

    initialize = function(body, exit, condition, ivar, iter) {
      super$initialize(body, exit, condition)
      self$set_ivar(ivar)
      self$set_iter(iter)
    },

    set_ivar = function(value) {
      value$parent = self
      self$ivar = value
    },

    set_iter = function(value) {
      value$parent = self
      self$iter = value
    }
  )
)
