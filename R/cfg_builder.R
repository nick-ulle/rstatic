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
      self$insert_block = cfg$entry
      self$loop_stack = Stack$new()

      return (self)
    },

    new_block = function() {
      id = self$cfg$add_vertex()
      self$cfg[[id]] = BasicBlock$new()

      return (id)
    },

    create_br = function(dest, src = self$insert_block) {
      self$cfg$add_edge(src, dest)

      self$cfg[[src]]$terminator = BrTerminator$new(dest)
      self$insert_block = dest
      invisible (NULL)
    },

    create_cond_br = function(true, false, condition, src = self$insert_block)
    {
      self$cfg$add_edge(src, true)
      self$cfg$add_edge(src, false)

      self$cfg[[src]]$terminator = CondBrTerminator$new(true, false, condition)
      self$insert_block = true
      invisible (NULL)
    },

    create_iter = function(body, exit, ivar, iter, src = self$insert_block) {
      self$cfg$add_edge(src, body)
      self$cfg$add_edge(src, exit)

      self$cfg[[src]]$terminator = IterTerminator$new(body, exit, ivar, iter)
      self$insert_block = body
      invisible (NULL)
    },

    create_ret = function(src = self$insert_block) {
      # NOTE: This function doesn't actually create a RetTerminator. Instead,
      # it creates an unconditional branch to the the exit block. This way
      # there is always exactly one exit block, and the correct return value
      # arrives there through SSA.

      self$cfg$add_edge(src, self$cfg$exit)

      self$create_br(src = src, self$cfg$exit)
      self$insert_block = NA_character_
      invisible (NULL)
    },

    loop_push = function(entry, exit) {
      self$loop_stack$push(c(entry, exit))
      invisible (NULL)
    },

    loop_pop = function() {
      return (self$loop_stack$pop())
    },

    create_break = function(src = self$insert_block) {
      exit = self$loop_stack$peek()[[2]]
      self$create_br(src = src, exit)
      self$insert_block = NA_character_
      invisible (NULL)
    },

    create_next = function(src = self$insert_block) {
      entry = self$loop_stack$peek()[[1]]
      self$create_br(src = src, entry)
      self$insert_block = NA_character_
      invisible (NULL)
    },

    append = function(node) {
      self$cfg[[self$insert_block]]$append(node)

      invisible (NULL)
    }
  )
)
