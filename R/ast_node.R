# Classes that represent AST nodes.

#' @export
ASTNode = R6::R6Class("ASTNode",
  "private" = list(
    deep_clone = function(name, value) {
      # Cloning a parent node could result in an infinite cloning loop.
      # Instead, each object's $copy() method is responsible for setting
      # $parent on immediate children after the object has been cloned.
      if (name == "parent")
        NULL
      else
        .copyAST(value)
    }
  ),

  "public" = list(
    parent = NULL,

    initialize = function(parent = NULL) {
      self$parent = parent
    },

    copy = function(...) {
      cloned = self$clone(deep = TRUE)

      # Reparent ASTNode objects that aren't in the "parent" field.
      names = setdiff(names(cloned), c("parent", ".__enclos_env__"))
      for (name in names) {
        if ( bindingIsActive(name, cloned) || is.function(cloned[[name]]) )
          next

        cloned[[name]] = .reparentAST(cloned[[name]], cloned)
      }

      cloned
    }
  )
)



# Containers
# --------------------

#' @export
Brace = R6::R6Class("Brace", inherit = ASTNode,
  "public" = list(
    .body = NULL,
    is_paren = FALSE,

    initialize = function(body = list(), is_paren = FALSE, parent = NULL) {
      super$initialize(parent)
      self$body = body
      self$is_paren = is_paren
    }
  ),

  "active" = list(
    body = function(value) {
      if (missing(value))
        return (self$.body)

      self$.body = .reparentAST(value, self)
    }
  )
)



# Control Flow
# --------------------

#' @export
Next = R6::R6Class("Next", inherit = ASTNode)

#' @export
Break = R6::R6Class("Break", inherit = ASTNode)

#' @export
If = R6::R6Class("If", inherit = ASTNode,
  "public" = list(
    .condition = NULL,
    .true = NULL,
    .false = NULL,

    initialize = function(condition, true, false = NULL, parent = NULL) {
      super$initialize(parent)

      self$condition = condition
      self$true = true
      self$false = false
    }
  ),

  "active" = list(
    condition = function(value) {
      if (missing(value))
        return (self$.condition)

      self$.condition = .reparentAST(value, self)
    },

    true = function(value) {
      if (missing(value))
        return (self$.true)

      self$.true = .reparentAST(value, self)
    },

    false = function(value) {
      if (missing(value))
        return (self$.false)

      self$.false = .reparentAST(value, self)
    }
  )
)

#' @export
For = R6::R6Class("For", inherit = ASTNode,
  "public" = list(
    .ivar = NULL,
    .iter = NULL,
    .body = NULL,

    initialize = function(ivar, iter, body, parent = NULL) {
      super$initialize(parent)

      self$ivar = ivar
      self$iter = iter
      self$body = body
    }
  ),

  "active" = list(
    ivar = function(value) {
      if (missing(value))
        return (self$.ivar)

      self$.ivar = .reparentAST(value, self)
    },

    iter = function(value) {
      if (missing(value))
        return (self$.iter)

      self$.iter = .reparentAST(value, self)
    },

    body = function(value) {
      if (missing(value))
        return (self$.body)

      self$.body = .reparentAST(value, self)
    }
  )
)

#' @export
While = R6::R6Class("While", inherit = ASTNode,
  "public" = list(
    .condition = NULL,
    .body = NULL,
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
      if (missing(value))
        return (self$.condition)

      self$.condition = .reparentAST(value, self)
    },

    body = function(value) {
      if (missing(value))
        return (self$.body)

      self$.body = .reparentAST(value, self)
    }
  )
)


# Calls
# --------------------

#' export
Application = R6::R6Class("Application", inherit = ASTNode,
  "public" = list(
    .args = NULL,

    initialize = function(args = list(), parent = NULL) {
      super$initialize(parent)

      self$args = args
    }
  ),

  "active" = list(
    args = function(value) {
      if (missing(value))
        return (self$.args)

      self$.args = .reparentAST(value, self)
    }
  )
)

#' @export
Return = R6::R6Class("Return", inherit = Application,
  "public" = list(
    initialize = function(args = list(), parent = NULL) {
      if (!is.list(args))
        args = list(args)

      super$initialize(args, parent)
    }
  )
)

#' @export
Call = R6::R6Class("Call", inherit = Application,
  "public" = list(
    .fn = NULL,

    initialize = function(fn, args = list(), parent = NULL) {
      super$initialize(args, parent)

      self$fn = fn
    }
  ),

  "active" = list(
    fn = function(value) {
      if (missing(value))
        return (self$.fn)

      # NOTE: fn could be a Symbol, Function, Primitive, or Call.
      if (!inherits(value, "ASTNode"))
        value = Symbol$new(value)

      self$.fn = .reparentAST(value, self)
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
Namespace = R6::R6Class("Namespace", inherit = Call)

#' @export
Subset = R6::R6Class("Subset", inherit = Call)

# Functions
# --------------------

#' @export
Callable = R6::R6Class("Callable", inherit = ASTNode,
  "public" = list(
    .params = NULL,

    initialize = function(params, parent = NULL) {
      super$initialize(parent)

      self$params = params
    }
  ),

  "active" = list(
    params = function(value) {
      if (missing(value))
        return (self$.params)

      self$.params = .reparentAST(value, self)
    }
  )
)

#' @export
Function = R6::R6Class("Function", inherit = Callable,
  "public" = list(
    .body = NULL,
    cfg = NULL,
    ssa = NULL,
    global_uses = character(0),

    initialize = function(params, body, parent = NULL) {
      super$initialize(params, parent)

      self$body = body
    }
  ),

  "active" = list(
    body = function(value) {
      if (missing(value))
        return (self$.body)

      self$.body = .reparentAST(value, self)
    }
  )
)

#' @export
Primitive = R6::R6Class("Primitive", inherit = Callable,
  "public" = list(
    .fn = NULL,

    initialize = function(params, fn, parent = NULL) {
      super$initialize(params, parent)

      self$fn = fn
    }
  ),

  "active" = list(
    fn = function(value) {
      if (missing(value))
        return (self$.fn)

      if (!inherits(value, "Symbol"))
        value = Symbol$new(value)

      self$.fn = .reparentAST(value, self)
    }
  )
)



# Assignment
# --------------------

#' @export
Assign = R6::R6Class("Assign", inherit = ASTNode,
  "public" = list(
    .write = NULL,
    .read = NULL,

    initialize = function(write, read, parent = NULL) {
      super$initialize(parent)

      self$write = write
      self$read = read
    }
  ),

  "active" = list(
    write = function(value) {
      if (missing(value))
        return (self$.write)

      self$.write = .reparentAST(value, self)
    },

    read = function(value) {
      if (missing(value))
        return (self$.read)

      self$.read = .reparentAST(value, self)
    }
  )
)


#' @export
Replacement = R6::R6Class("Replacement", inherit = Assign,
  "public" = list(
    initialize = function(write, fn, args, parent = NULL) {
      if (!inherits(fn, "ASTNode")) {
        fn = as.character(fn)

        if (!endsWith(fn, "<-"))
          fn = paste0(fn, "<-")

        fn = Symbol$new(fn)
      }

      read = Call$new(fn, args)

      super$initialize(write, read, parent)
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

      if (!inherits(value, "Symbol"))
        value = Symbol$new(value)

      self$.write = .reparentAST(value, self)
    }
  )
)



# Symbols
# --------------------

#' @export
Symbol = R6::R6Class("Symbol", inherit = ASTNode,
  "public" = list(
    basename = NULL,
    ssa_number = NULL,
    namespace = NULL,
    namespace_fn = NULL,

    initialize = function(
      basename, ssa_number = NA_integer_,
      namespace = NA_character_, namespace_fn = NULL,
      parent = NULL
    ) {
      if ( !(is.character(basename) || is.symbol(basename)) )
        stop("Symbol basename must be a character or a name.", call. = FALSE)

      super$initialize(parent)
      self$basename = as.character(basename)
      self$ssa_number = ssa_number

      self$namespace = namespace
      self$namespace_fn = namespace_fn
    }
  ),

  "active" = list(
    name = function() {
      ssa_number = self$ssa_number
      if (is.na(ssa_number))
        return (self$basename)

      sprintf("%s_%i", self$basename, ssa_number)
    }
  )
)

#' @export
Parameter = R6::R6Class("Parameter", inherit = Symbol,
  "public" = list(
    .default = NULL,

    initialize = function(name, default = NULL, ssa = NA_integer_,
      parent = NULL)
    {
      super$initialize(name, ssa, parent)

      self$default = default
    }
  ),

  "active" = list(
    default = function(value) {
      if (missing(value))
        return (self$.default)

      self$.default = .reparentAST(value, self)
    }
  )
)



# Literals
# --------------------

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


# Cloning Methods
# --------------------
.copyAST         = function(value) UseMethod(".copyAST")
#' @export
.copyAST.ASTNode = function(value) value$copy()
#' @export
.copyAST.list    = function(value) lapply(value, .copyAST)
#' @export
.copyAST.R6      = function(value) value$clone(deep = TRUE)
#' @export
.copyAST.default = function(value) value

.reparentAST = function(value, parent)
  UseMethod(".reparentAST")

#' @export
.reparentAST.ASTNode = function(value, parent) {
  value$parent = parent
  value
}

#' @export
.reparentAST.list = function(value, parent) {
  lapply(value, .reparentAST, parent)
}

#' @export
.reparentAST.default = function(value, parent)
  value
