# FIXME: Copying Terminators doesn't handle circular references correctly.

Terminator = R6::R6Class("Terminator", inherit = ASTNode,
  "public" = list(
    successors = function() integer(0)
  )
)

#' @export
RetTerminator = R6::R6Class("RetTerminator", inherit = Terminator,
  "public" = list(
    .value = NULL,

    initialize = function(value = Symbol$new("._return_")) {
      self$value = value
    }
  ),

  "active" = list(
    value = function(value) {
      if (missing(value))
        return (self$.value)

      self$.value = .reparentAST(value, self)
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
    # FIXME: Should true and false here have setters?
    true = NULL,
    false = NULL,
    .condition = NULL,

    initialize = function(true, false, condition) {
      self$true = true
      self$false = false
      self$condition = condition
    },

    successors = function() {
      c(self$true, self$false)
    }
  ),

  "active" = list(
    condition = function(value) {
      if (missing(value))
        return (self$.condition)

      self$.condition = .reparentAST(value, self)
    }
  )
)

#' @export
IterTerminator = R6::R6Class("IterTerminator", inherit = CondBrTerminator,
  public = list(
    .ivar = NULL,
    .iter = NULL,

    initialize = function(body, exit, condition, ivar, iter) {
      super$initialize(body, exit, condition)
      self$ivar = ivar
      self$iter = iter
    }
  ),

  "active" = list(
    ivar = function(value) {
      if (missing(value))
        return (self$.ivar)

      self$.ivar = .reparentAST(value, self)
    },

    iter = function(value) {
      if (missing(value))
        return (self$.iter)

      self$.iter = .reparentAST(value, self)
    }
  )
)
