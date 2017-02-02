
#' @include stack.R
NULL

NameStack = R6::R6Class("NameStack",
  "private" = list(
    name_gen = NULL,
    name_stack = list(),
    local_stack = NULL,
    local = character(0)
  ),

  "public" = list(
    initialize = function() {
      private$name_gen = NameGenerator$new()
      private$local_stack = Stack$new(type = "list")
    },

    save_locals = function() {
      # Save locals on the stack.
      private$local_stack$push(private$local)
      private$local = character(0)

      invisible (self)
    },

    clear_locals = function() {
      # Restore locals from the stack, then clear them.
      local = private$local_stack$pop()
      lapply(local,
        function(base) private$name_stack[[base]]$pop()
      )

      invisible (self)
    },

    get_name = function(base) {
      # Peek at a name on the stack.
      idx = match(base, names(private$name_stack))
      if (is.na(idx))
        # No names defined, so use base_0.
        # FIXME: This is a hack. The base_0 name should be pushed on the stack
        # without altering the locals (base_0 is assumed global).
        return (sprintf("%s_0", base))

      private$name_stack[[idx]]$peek()
    },

    new_name = function(base) {
      # Push a new name onto the stack and mark the base as local.
      name = private$name_gen$get(base)

      if (base %in% private$local) {
        private$name_stack[[base]]$pop()
      } else {
        private$local = union(base, private$local)

        if ( !(base %in% names(private$name_stack)) )
          private$name_stack[[base]] = Stack$new(type = "character")
      }
      private$name_stack[[base]]$push(name)

      return (name)
    }
  )
)


#' Generate Unique Variable Names
#'
NameGenerator = R6::R6Class("NameGenerator",
  "private" = list(
    counter = integer(0)
  ),

  "public" = list(
    get = function(base) {
      if (base %in% names(private$counter)) {
        counter = private$counter[[base]] + 1L
      } else {
        counter = 1L
      }
      
      private$counter[[base]] = counter

      return (sprintf("%s_%i", base, counter))
    }
  )
)

