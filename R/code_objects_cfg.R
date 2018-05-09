# Code objects that only appear in the control flow graph.

#' @include code_objects_ast.R
NULL

#' @export
Block = R6::R6Class("Block", inherit = Container,
  "public" = list(
    id = NA_character_,
    depth = NA_integer_,
    .phi = NULL,

    initialize = function(body = list(), id = NA_character_,
      depth = NA_integer_, phi = list(), parent = NULL)
    {
      super$initialize(body, parent)

      self$id = id
      self$depth = depth
      self$phi = phi
    },

    set_phi = function(phi) {
      self$.phi[[phi$write$basename]] = set_parent(phi, self)

      invisible(NULL)
    }
  ),

  "active" = list(
    phi = function(value) {
      if (missing(value))
        return (self$.phi)

      self$.phi = set_parent(value, self)
    }
  )
)

#' @export
Label = R6::R6Class("Label", inherit = ASTNode,
  "public" = list(
    name = NA,
    
    initialize = function(name = NA, parent = NULL) {
      self$name = name
    }
  )
)

#' @export
Phi = R6::R6Class("Phi", inherit = ASTNode,
  # FIXME: Phi and Assign should probably have a common superclass for
  # variable-changing instructions. The Replacement class is also related.
  "public" = list(
    .write = NULL,
    blocks = integer(0),
    read = list(),

    initialize = function(write, parent = NULL) {
      super$initialize(parent)

      self$write = write
    },

    set_read = function(block, value) {
      idx = match(block, self$blocks)
      if (is.na(idx)) {
        idx = length(self$blocks) + 1L
        self$blocks[[idx]] = block
      }
      self$read[[idx]] = value
      names(self$read)[[idx]] = block
    },

    get_read = function(block) {
      idx = match(block, self$blocks)
      self$read[[idx]]
    }
  ),

  "active" = list(
    write = function(value) {
      if (missing(value))
        return (self$.write)

      if (!is(value, "Symbol"))
        value = Symbol$new(value)

      self$.write = set_parent(value, self)
    }
  )
)
