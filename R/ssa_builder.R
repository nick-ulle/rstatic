SSABuilder = R6::R6Class("SSABuilder",
  "public" = list(
    defs = integer(0),
    name_counter = NULL,
    ssa = NULL,

    initialize = function() {
      self$name_counter = Counter$new()
      self$ssa = DataFlowGraph$new()
    },

    get_live_def = function(basename) {
      self$defs[basename]
    },

    new_def = function(basename) {
      n = self$name_counter$increment(basename)
      self$defs[basename] = n
    },

    register_use = function(name, at) {
      id = self$ssa$add_use(at)

      # Add incoming edge.
      self$ssa$add_edge(name, id)

      invisible(NULL)
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
      #initial = list(kill = character(0), gen = character(0))
      #c(, reads) := live_variables_killgen(at, initial)

      defs = names(self$ssa)
      for (r in reads)
        self$ssa$add_edge(r, name)

      invisible(NULL)
    }
  )
)
