#' CFG Builder
#'
#' This class is used by \code{to_cfg} to build a control flow graph.
#'
#' @export
CFGBuilder = R6::R6Class("CFGBuilder",
  "public" = list(
    insert_block = NA_character_,
    cfg = NULL,
    loop_stack = NULL,

    #dom_tree = NULL,
    #usedef = NULL,

    initialize = function(cfg) {
      self$cfg = cfg
      self$loop_stack = Stack$new()

      # Assume this is a function.
      # NOTE: This setup step should really be handled by the CFG.
      entry = self$cfg$add_vertex()
      self$cfg$entry = entry
      self$cfg[[entry]] = FnEntryBlock$new()

      exit = self$cfg$add_vertex()
      self$cfg$exit = exit
      self$cfg[[exit]] = FnExitBlock$new()

      self$insert_block = entry
      return (self)
    },

    new_block = function() {
      id = self$cfg$add_vertex()
      self$cfg[[id]] = BasicBlock$new()

      return (id)
    },

    jump = function(to, from = self$insert_block) {
      self$cfg$add_edge(from, to)

      self$cfg[[from]]$terminator = BranchInst$new(to)
      self$cfg[[to]]$add_predecessor(from)
      self$insert_block = to
      return (self)
    },

    branch = function(to_t, to_f, condition, from = self$insert_block) {
      self$cfg$add_edge(from, to_t)
      self$cfg$add_edge(from, to_f)

      self$cfg[[from]]$terminator = BranchInst$new(to_t, to_f, condition)
      self$cfg[[to_t]]$add_predecessor(from)
      self$cfg[[to_f]]$add_predecessor(from)
      self$insert_block = to_t
      return (self)
    },

    iterate = function(to_t, to_f, ivar, iter, from = self$insert_block) {
      self$cfg$add_edge(from, to_t)
      self$cfg$add_edge(from, to_f)

      self$cfg[[from]]$terminator = IterateInst$new(to_t, to_f, ivar, iter)
      self$cfg[[to_t]]$add_predecessor(from)
      self$cfg[[to_f]]$add_predecessor(from)
      self$insert_block = to_t
      return (self)
    },

    fn_return = function(from = self$insert_block) {
      # FIXME:
      if (is.na(self$exit_fn))
        stop("no exit block to return to.",
          "\n  Did you attempt to use return() outside of a function?")

      self$jump(from = from, self$exit_fn)
      self$insert_block = NA_character_
      return (self)
    },

    loop_push = function(entry, exit) {
      self$loop_stack$push(c(entry, exit))
      return (self)
    },

    loop_pop = function() {
      return (self$loop_stack$pop())
    },

    loop_break = function(from = self$insert_block) {
      exit = self$loop_stack$peek()[[2]]
      self$jump(from = from, exit)
      self$insert_block = NA_character_
      return (self)
    },

    loop_next = function(from = self$insert_block) {
      entry = self$loop_stack$peek()[[1]]
      self$jump(from = from, entry)
      self$insert_block = NA_character_
      return (self)
    },

    append = function(node) {
      self$cfg[[self$insert_block]]$append(node)

      invisible (NULL)
    }
  )
)
