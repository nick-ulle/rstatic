
SSABuilder = R6::R6Class("SSABuilder",
  "public" = list(
    name_gen = NULL,
    name_stack = list(),
    local_stack = NULL,
    local = character(0),
    ssa = NULL,

    initialize = function() {
      self$ssa = FlowGraph$new()
      self$name_gen = NameGenerator$new()
      self$local_stack = Stack$new(type = "list")
    },

    save_local_defs = function() {
      # Save local definitions so they can be cleared later.
      self$local_stack$push(self$local)
      self$local = character(0)

      invisible (self)
    },

    clear_local_defs = function() {
      # Clear saved local definitions.
      local = self$local_stack$pop()
      lapply(local,
        function(base) self$name_stack[[base]]$pop()
      )

      invisible (self)
    },

    get_live_def = function(base) {
      ns = self$name_stack[[base]]
      if (is.null(ns) || ns$is_empty)
        # Base has no definitions, so return NA.
        return (NA_integer_)

      ns$peek()
    },

    new_def = function(base) {
      # Check whether base already has a definition in this block.
      if (base %in% self$local) {
        self$name_stack[[base]]$pop()

      } else {
        # NOTE: It would probably be okay to use c() instead of union() here.
        self$local = union(base, self$local)

        # Check whether base has a name stack.
        if ( !(base %in% names(self$name_stack)) )
          self$name_stack[[base]] = Stack$new(type = "integer")
      }

      # Push a new number onto the stack.
      n = self$name_gen$get(base)
      self$name_stack[[base]]$push(n)

      return (n)
    },

    register_use = function(name, at) {
      id = self$ssa$add_vertex()
      self$ssa[[id]] = at

      # FIXME: What about globals?
      if (name %in% names(self$ssa))
        self$ssa$add_edge(name, id)

      invisible (NULL)
    },

    register_def = function(name, at) {
      if (is.null(self$ssa[[name]])) {
        # Add node to the graph.
        id = self$ssa$add_vertex(name)
        self$ssa[[id]] = at
      } else {
        stop(sprintf("symbol '%s' already defined.", name))
      }

      invisible (NULL)
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

