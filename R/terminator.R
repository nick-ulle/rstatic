Terminator = R6::R6Class("Terminator",
  "public" = list(
    successors = function() integer(0)
  )
)

#' @export
RetTerminator = R6::R6Class("RetTerminator", inherit = Terminator,
  "public" = list(
    value = NULL,

    initialize = function(value = Symbol$new("._return_")) {
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
      self$condition = condition
    },

    successors = function() {
      c(self$true, self$false)
    }
  )
)

#' @export
IterTerminator = R6::R6Class("IterTerminator", inherit = CondBrTerminator,
  public = list(
    ivar = NULL,
    iter = NULL,

    initialize = function(body, exit, ivar, iter) {
      super$initialize(body, exit, NULL)
      self$ivar = ivar
      self$iter = iter
    }
  )
)
