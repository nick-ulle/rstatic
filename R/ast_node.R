
# Classes that represent AST nodes.

#' @export
ASTNode = R6::R6Class("ASTNode",
  "public" = list(
    parent = NULL,
    initialize = function(parent = NULL) {
      self$parent = parent
    }
  )
)

#' @export
Next = R6::R6Class("Next", inherit = ASTNode)

#' @export
Break = R6::R6Class("Break", inherit = ASTNode)

#' @export
If = R6::R6Class("If", inherit = ASTNode,
  "public" = list(
    initialize = function(condition, true, false = NULL, parent = NULL)
    { 
      super$initialize(parent)

      self$condition = condition
      self$true = true
      self$false = false
    }
  ),

  "active" = list(
    condition = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.condition = value
      }

      return (private$.condition)
    },

    true = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.true = value
      }

      return (private$.true)
    },

    false = function(value) {
      if (!missing(value)) {
        if (!is.null(value))
          value$parent = self
        private$.false = value
      }

      return (private$.false)
    }
  ),

  "private" = list(
    .condition = NULL,
    .true = NULL,
    .false = NULL
  )
)

#' @export
For = R6::R6Class("For", inherit = ASTNode,
  "public" = list(
    initialize = function(ivar, iter, body, parent = NULL) {
      super$initialize(parent)

      self$ivar = ivar
      self$iter = iter
      self$body = body
    }
  ),

  "active" = list(
    ivar = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.ivar = value
      }

      return (private$.ivar)
    },

    iter = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.iter = value
      }

      return (private$.iter)
    },

    body = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.body = value
      }

      return (private$.body)
    }
  ),

  "private" = list(
    .ivar = NULL,
    .iter = NULL,
    .body = NULL
  )
)

#' @export
While = R6::R6Class("While", inherit = ASTNode,
  "public" = list(
    is_repeat = FALSE,
    initialize = function(condition, body, is_repeat = FALSE, parent = NULL) {
      super$initialize(parent)

      self$condition = condition
      self$body = body
      self$is_repeat = is_repeat
    }
  ),

  "active" = list(
    condition = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.condition = value
      }

      return (private$.condition)
    },

    body = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.body = value
      }

      return (private$.body)
    }
  ),

  "private" = list(
    .condition = NULL,
    .body = NULL
  )
)

#' @export
Assign = R6::R6Class("Assign", inherit = ASTNode,
  "public" = list(
    initialize = function(write, read, parent = NULL) {
      super$initialize(parent)

      self$write = write
      self$read = read
    }
  ),

  "active" = list(
    write = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.write = value
      }

      return (private$.write)
    },

    read = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.read = value
      }

      return (private$.read)
    }
  ),

  "private" = list(
    .write = NULL,
    .read = NULL
  )
)


#' @export
Phi = R6::R6Class("Phi", inherit = ASTNode,
  # FIXME: Phi and Assign should probably have a common superclass for
  # variable-changing instructions. The Replacement class is also related.
  "public" = list(
    initialize = function(write = NULL, read = NULL, parent = NULL) {
      super$initialize(parent)

      self$write = write
      self$read = read
    }
  ),

  "active" = list(
    write = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.write = value
      }

      return (private$.write)
    },

    read = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.read = value
      }

      return (private$.read)
    }
  ),

  "private" = list(
    .write = NULL,
    .read = NULL # needs to track (name, block)
  )
)

#' export
Dispatch = R6::R6Class("Dispatch", inherit = ASTNode,
  "public" = list(
    initialize = function(args = list(), parent = NULL) {
      super$initialize(parent)

      self$args = args
    }
  ),

  "active" = list(
    args = function(value) {
      if (!missing(value)) {
        for (x in value)
          x$parent = self
        private$.args = value
      }

      return (private$.args)
    }
  ),

  "private" = list(
    .args = NULL
  )
)

#' @export
Return = R6::R6Class("Return", inherit = Dispatch,
  "public" = list(
    is_invisible = FALSE,
    initialize = function(args = list(), is_invisible = FALSE, parent = NULL) {
      super$initialize(args, parent)

      self$is_invisible = is_invisible
    }
  )
)

#' @export
Call = R6::R6Class("Call", inherit = Dispatch,
  "public" = list(
    func = NULL,
    initialize = function(func, args = list(), parent = NULL) {
      super$initialize(args, parent)

      # FIXME: Set parent for func.
      self$func = func
    }
  ),

)

#' @export
Replacement = R6::R6Class("Replacement", inherit = Call,
  "public" = list(
    initialize = function(func, args = list(), parent = NULL) {
      func = sprintf("%s<-", as.character(func))
      super$initialize(func, args, parent)
    }
  )
)

#' @export
Internal = R6::R6Class("Internal", inherit = Call,
  "public" = list(
    initialize = function(args = NULL, parent = NULL) {
      super$initialize(".Internal", args, parent)
    }
  )
)


#' @export
Symbol = R6::R6Class("Symbol", inherit = ASTNode,
  "public" = list(
    name = NULL,
    type = NULL,
    initialize = function(name, type = NULL, parent = NULL) {
      super$initialize(parent)
      self$name = name
      self$type = type
    }
  )
)

#' @export
Parameter = R6::R6Class("Parameter", inherit = Symbol,
  "public" = list(
    initialize = function(name, default = NULL, type = NULL, parent = NULL) {
      super$initialize(name, type, parent)

      self$default = default
    }
  ),

  "active" = list(
    default = function(value) {
      if (!missing(value)) {
        if (!is.null(value))
          value$parent = self
        private$.default = value
      }

      return (private$.default)
    }
  ),

  "private" = list(
    .default = NULL
  )
)

#' @export
Callable = R6::R6Class("Callable", inherit = ASTNode,
  "public" = list(
    initialize = function(params, parent = NULL) {
      super$initialize(parent)

      self$params = params
    }
  ),

  "active" = list(
    params = function(value) {
      if (!missing(value)) {
        for (x in value)
          x$parent = self
        private$.params = value
      }

      return (private$.params)
    }
  ),

  "private" = list(
    .params = NULL
  )
)

#' @export
Function = R6::R6Class("Function", inherit = Callable,
  "public" = list(
    initialize = function(params, body, parent = NULL) {
      super$initialize(params, parent)

      self$body = body
    }
  ),

  "active" = list(
    body = function(value) {
      if (!missing(value)) {
        value$parent = self
        private$.body = value
      }

      return (private$.body)
    }
  ),

  "private" = list(
    .body = NULL
  )
)

# FIXME:
#' @export
Primitive = R6::R6Class("Primitive", inherit = Callable,
  "public" = list(
    name = NULL,
    initialize = function(name, params, parent = NULL) {
      super$initialize(params, parent)
      self$name = name
    }
  )
)

#' @export
Brace = R6::R6Class("Brace", inherit = ASTNode,
  "public" = list(
    is_paren = FALSE,
    initialize = function(body = list(), is_paren = FALSE, parent = NULL) {
      super$initialize(parent)
      self$body = body
      self$is_paren = is_paren
    }
  ),

  "active" = list(
    body = function(value) {
      if (!missing(value)) {
        for (x in value)
          x$parent = self
        private$.body = value
      }

      return (private$.body)
    }
  ),

  "private" = list(
    .body = NULL
  )
)

#' @export
Literal = R6::R6Class("Literal", inherit = ASTNode,
  "public" = list(
    value = NULL,
    initialize = function(value, parent = NULL) {
      super$initialize(parent)
      self$value = value
    }
  )
)

#' @export
Null = R6::R6Class("Null", inherit = Literal,
  "public" = list(
    initialize = function(parent = NULL) {
      super$initialize(NULL, parent)
    }
  )
)

#' @export
Logical = R6::R6Class("Logical", inherit = Literal)

#' @export
Integer = R6::R6Class("Integer", inherit = Literal)

#' @export
Numeric = R6::R6Class("Numeric", inherit = Literal)

#' @export
Complex = R6::R6Class("Complex", inherit = Literal)

#' @export
Character = R6::R6Class("Character", inherit = Literal)
