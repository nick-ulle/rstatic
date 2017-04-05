

NameStack = R6::R6Class("NameStack",
  "private" = list(
    name_gen = NULL,
    name_stack = list(),
    local_stack = NULL,
    local = character(0)
  ),

  "public" = list(
    usedef = NULL,

    initialize = function() {
      private$name_gen = NameGenerator$new()
      private$local_stack = Stack$new(type = "list")
    },

    save_local_defs = function() {
      # Save local definitions so they can be cleared later.
      private$local_stack$push(private$local)
      private$local = character(0)

      invisible (self)
    },

    clear_local_defs = function() {
      # Clear saved local definitions.
      local = private$local_stack$pop()
      lapply(local,
        function(base) private$name_stack[[base]]$pop()
      )

      invisible (self)
    },

    get_live_def = function(base) {
      ns = private$name_stack[[base]]
      if (is.null(ns) || ns$is_empty)
        # Base has no definitions, so return NA.
        return (NA_integer_)

      ns$peek()
    },

    new_def = function(base) {
      # Check whether base already has a definition in this block.
      if (base %in% private$local) {
        private$name_stack[[base]]$pop()

      } else {
        # NOTE: It would probably be okay to use c() instead of union() here.
        private$local = union(base, private$local)

        # Check whether base has a name stack.
        if ( !(base %in% names(private$name_stack)) )
          private$name_stack[[base]] = Stack$new(type = "integer")
      }

      # Push a new number onto the stack.
      n = private$name_gen$get(base)
      private$name_stack[[base]]$push(n)

      return (n)
    },

    register_use = function(name, at) {
      usedef = self$usedef[[name]]
      if (is.null(usedef)) {
        usedef = list(def = NULL, use = list(at))
      } else {
        usedef[[2]] = append(usedef[[2]], at)
      }
      self$usedef[[name]] = usedef

      return (self)
    },

    register_def = function(name, at) {
      usedef = self$usedef[[name]]
      if (is.null(usedef)) {
        usedef = list(def = at, use = list())
      } else {
        usedef[[1]] = at
      }
      self$usedef[[name]] = usedef

      return (self)
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

      return (counter)
    }
  )
)

