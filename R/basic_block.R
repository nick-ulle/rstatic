# Classes that represent basic blocks.


#' @export
BasicBlock = R6::R6Class("BasicBlock",
  "private" = list(
    deep_clone = function(name, value) {
      switch(name,
        "body" = lapply(value, function(v) v$copy()),
        "phi" = lapply(value, function(v) v$copy()),
        if (inherits(value, "R6")) value$clone(deep = TRUE)
        else value
      )
    }
  ),

  "public" = list(
    body = NULL,
    phi = NULL,
    terminator = NULL,
    predecessors = integer(0),

    initialize = function(body = list()) {
      for (node in body)
        node$parent = self
      self$body = body

      self$phi = list()
      return (self)
    },

    copy = function() {
      cloned = self$clone(deep = TRUE)

      for (node in cloned$body)
        node$parent = cloned

      for (node in cloned$phi)
        node$parent = cloned

      return (cloned)
    },

    add_predecessor = function(block) {
      self$predecessors = union(self$predecessors, block)
      return (self)
    },

    append = function(node, after = length(self$body)) {
      node$parent = self
      if (inherits(node, "Phi")) {
        self$phi = append(self$phi, node)
      } else {
        self$body = append(self$body, node, after)
      }
      return (self)
    }
  ),

  "active" = list(
    is_terminated = function() {
      return (!is.null(self$terminator))
    }

    , successors = function() {
      if (self$is_terminated)
        return (self$terminator$successors)
      return (integer(0))
    }
  )
)


#' @export
FnEntryBlock = R6::R6Class("FnEntryBlock", inherit = BasicBlock,
  "public" = list(
    params = list(),

    set_params = function(value) {
      for (v in value)
        v$parent = self

      self$params = value
    }
  )
)

#' @export
FnExitBlock = R6::R6Class("FnExitBlock", inherit = BasicBlock)


has_phi = function(block, x) {
  # Check if there's a phi-function in block for name x.
  match = vapply(block$phi, function(node) {
    return (node$write == x)
  }, logical(1))

  any(match)
}


Terminator = R6::R6Class("Terminator")

#' @export
BranchInst = R6::R6Class("BranchInst", inherit = Terminator,
  public = list(
    true = NULL, # FIXME:
    false = NULL,
    condition = NULL,

    initialize = function(to_t, to_f = NULL, condition = NULL) {
      self$true = to_t
      self$false = to_f
      self$condition = condition
    }
  ),

  active = list(
    successors = function() {
      return (c(self$true, self$false))
    }
  )
)

#' @export
IterateInst = R6::R6Class("IterateInst", inherit = Terminator,
  public = list(
    body = NULL,
    exit = NULL,
    ivar = NULL,
    iter = NULL,

    initialize = function(to_body, to_exit, ivar, iter) {
      self$body = to_body
      self$exit = to_exit
      self$ivar = ivar
      self$iter = iter
    }
  ),

  active = list(
    successors = function() {
      c(self$body, self$exit)
    }
  )
)
