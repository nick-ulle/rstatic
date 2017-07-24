
SSABuilder = R6::R6Class("SSABuilder",
  "public" = list(
    register_uses = TRUE,
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
        function(basename) self$name_stack[[basename]]$pop()
      )

      invisible (self)
    },

    get_live_def = function(basename) {
      ns = self$name_stack[[basename]]
      if (is.null(ns) || ns$is_empty)
        # Base has no definitions, so return NA.
        return (NA_integer_)

      ns$peek()
    },

    new_def = function(basename) {
      # Check whether basename already has a definition in this block.
      if (basename %in% self$local) {
        self$name_stack[[basename]]$pop()

      } else {
        # NOTE: It would probably be okay to use c() instead of union() here.
        self$local = union(basename, self$local)

        # Check whether basename has a name stack.
        if ( !(basename %in% names(self$name_stack)) )
          self$name_stack[[basename]] = Stack$new(type = "integer")
      }

      # Push a new number onto the stack.
      n = self$name_gen$get(basename)
      self$name_stack[[basename]]$push(n)

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

    # A node can be a use and a def when one variable is used to define
    # another. We need to register the def and then add edges to the def from
    # all the older defs it uses.
    register_def = function(name, at, paramNames = character()) {
      if (is.null(self$ssa[[name]])) {
        # Add node to the graph.
        id = self$ssa$add_vertex(name)
        self$ssa[[id]] = at

        reads = collect_reads(at)
        defs = names(self$ssa)
        for (r in reads) {
          if (r %in% defs)
            self$ssa$add_edge(r, name)
          else if(!(r %in% paramNames)) {
            warning(sprintf("not adding global name '%s' to SSA graph.", r))
          }
        }

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
    get = function(basename) {
      if (basename %in% names(private$counter)) {
        counter = private$counter[[basename]] + 1L
      } else {
        counter = 1L
      }
      
      private$counter[[basename]] = counter

      return (counter)
    }
  )
)

