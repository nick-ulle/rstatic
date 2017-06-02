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
    phi = list(),
    body = NULL,
    terminator = NULL,
    name = NULL,

    initialize = function(name = NULL, body = list()) {
      for (node in body)
        node$parent = self
      self$body = body
      self$name = name

      return (self)
    },

    append = function(node, after = length(self$body)) {
      node$parent = self
      if (inherits(node, "Phi")) {
        self$phi = append(self$phi, node)
      } else {
        self$body = append(self$body, node, after)
      }

      invisible (NULL)
    },

    copy = function() {
      cloned = self$clone(deep = TRUE)

      for (node in cloned$phi)
        node$parent = cloned

      for (node in cloned$body)
        node$parent = cloned

      return (cloned)
    }
  )
)


has_phi = function(block, x) {
  # Check if there's a phi-function in block for name x.
  match = vapply(block$phi, function(node) {
    return (node$write$base == x)
  }, logical(1))

  any(match)
}
