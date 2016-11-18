
Stack = R6::R6Class("Stack",
  private = list(
    buffer = NULL,
    len = 0,
    size = 0
  ),

  public = list(
    initialize = function(size = 16, type = "list") {
      private$size = size
      private$buffer = vector(type, private$size)
    },

    peek = function() {
      if (private$len == 0)
        return (NULL)

      return (private$buffer[[private$len]])
    },

    pop = function() {
      if (private$len == 0)
        return (NULL)

      x = private$buffer[[private$len]]
      private$buffer[[private$len]] = list(NULL)
      private$len = private$len - 1
      return (x)
    },

    push = function(x) {
      if (private$len == private$size) {
        private$size = 2 * private$size
        length(private$buffer) = private$size
      }

      private$len = private$len + 1
      private$buffer[[private$len]] = x
      return (self)
    }
  )
)
