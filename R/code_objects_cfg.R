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

    #has_phi = function(basenames) {
    #  basenames %in% names(self$.phi)
    #},

    insert_phi = function(basenames) {
      is_new = match(basenames, names(self$.phi), 0L) == 0L

      for (name in basenames[is_new])
        self$.phi[[name]] = Phi$new(Symbol$new(name))

      is_new
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
BlockList = R6::R6Class("BlockList", inherit = Container)

#' @export
Label = R6::R6Class("Label", inherit = ASTNode,
  "public" = list(
    name = NA,
    
    initialize = function(name = NA, parent = NULL) {
      self$name = name
    }
  )
)


# FIXME: This probably shouldn't be a container.
#' @export
Phi = R6::R6Class("Phi", inherit = Container,
  "public" = list(
    .write = NULL,
    ids = integer(0),

    initialize = function(write, parent = NULL) {
      super$initialize(parent = parent)

      self$write = write
    },

    set = function(id, value) {
      idx = match(id, self$ids, length(self$ids) + 1L)

      self$ids[[idx]] = id
      self$contents[[idx]] = value

      NULL
    },

    get = function(block) {
      self$contents[match(block, self$blocks)]
    }
  ),

  "active" = list(
    write = binding_factory(".write")
  )
)

##' @export
#Phi = R6::R6Class("Phi", inherit = ASTNode,
#  # FIXME: Phi and Assign should probably have a common superclass for
#  # variable-changing instructions. The Replacement class is also related.
#  "public" = list(
#    .write = NULL,
#    blocks = integer(0),
#    read = list(),
#
#    initialize = function(write, parent = NULL) {
#      super$initialize(parent)
#
#      self$write = write
#    },
#
#    set_read = function(block, value) {
#      idx = match(block, self$blocks)
#      if (is.na(idx)) {
#        idx = length(self$blocks) + 1L
#        self$blocks[[idx]] = block
#      }
#      self$read[[idx]] = value
#      names(self$read)[[idx]] = block
#    },
#
#    get_read = function(block) {
#      idx = match(block, self$blocks)
#      self$read[[idx]]
#    }
#  ),
#
#  "active" = list(
#    write = function(value) {
#      if (missing(value))
#        return (self$.write)
#
#      if (!is(value, "Symbol"))
#        value = Symbol$new(value)
#
#      self$.write = set_parent(value, self)
#    }
#  )
#)
