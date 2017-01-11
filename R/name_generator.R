
#' @include stack.R
NULL

#' Generate Unique Variable Names
#'
NameGenerator = R6::R6Class("NameGenerator",
  "public" = list(
    new_name = function(x) {
      counter = private$counter[[x]]
      stack = private$stack[[x]]

      if (is.null(counter)) {
        counter = 1L
        stack = Stack$new(type = "integer")
      } else {
        counter = counter + 1L
      }
      stack$push(counter)

      private$counter[[x]] = counter
      private$stack[[x]] = stack
      return (sprintf("%s_%i", x, counter))
    },

    peek = function(x) {
      stack = private$stack[[x]]
      if (is.null(stack))
        return (NA_integer_)

      return (stack$peek())
    },

    pop = function(x) {
      stack = private$stack[[x]]
      if (is.null(stack))
        return (NA_integer_)

      return (stack$pop())
    },

    reset = function() {
      private$counter = integer(0)
      private$stack = list()
    }
  ),

  "private" = list(
    counter = integer(0),
    stack = list()
  )
)

