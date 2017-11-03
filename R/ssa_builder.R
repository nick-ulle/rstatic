
SSABuilder = R6::R6Class("SSABuilder",
  "public" = list(
    register_uses = TRUE,
    name_counter = NULL,
    name_stack = list(),
    local_stack = NULL,
    local = character(0),
    ssa = NULL,

    initialize = function() {
      self$ssa = DataFlowGraph$new()
      self$name_counter = Counter$new()
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
      n = self$name_counter$increment(basename)
      self$name_stack[[basename]]$push(n)

      return (n)
    },

    register_use = function(name, at) {
      id = self$ssa$add_use(at)

      # Add incoming edge.
      self$ssa$add_edge(name, id)

      invisible (NULL)
    },

    # A node can be a use and a def when one variable is used to define
    # another. We need to register the def and then add edges to the def from
    # all the older defs it uses.
    register_def = function(name, basename, at) {
      if (name %in% names(self$ssa))
        stop(sprintf("symbol '%s' already defined.", name))

      id = self$ssa$add_def(at, basename, name)

      # Add incoming edges.
      reads = collect_reads(at)
      defs = names(self$ssa)
      for (r in reads)
        self$ssa$add_edge(r, name)

      invisible (NULL)
    }
  )
)
