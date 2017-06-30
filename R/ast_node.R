
# Classes that represent AST nodes.

#' @export
ASTNode = R6::R6Class("ASTNode",
  "private" = list(
    deep_clone = function(name, value) {
      # Don't clone parent nodes; each class' $copy() method is responsible for
      # reparenting cloned children.
      switch(name,
        "parent" = NULL
        , if (inherits(value, "ASTNode")) value$copy()
        else if (inherits(value, "R6")) value$clone(deep = TRUE)
        else value
      )
    }
  ),

  "public" = list(
    parent = NULL,

    initialize = function(parent = NULL) {
      self$parent = parent
    },

    copy = function() {
      cloned = self$clone(deep = TRUE)

      # Reparent any ASTNode objects that aren't in the "parent" field.
      for (name in names(cloned)) {
        field = cloned[[name]]
        if (is(field, "ASTNode") && name != "parent")
          field$parent = cloned
      }

      return (cloned)
    }
  )
)



# Containers
# --------------------

#' @export
Brace = R6::R6Class("Brace", inherit = ASTNode,
  "private" = list(
    deep_clone = function(name, value) {
      switch(name,
        "body" = lapply(value, function(v) v$copy())
        , super$deep_clone(name, value)
      )
    }
  ),

  "public" = list(
    body = NULL,
    is_paren = FALSE,

    initialize = function(body = list(), is_paren = FALSE, parent = NULL) {
      super$initialize(parent)
      self$set_body(body)
      self$is_paren = is_paren
    },

    copy = function() {
      cloned = super$copy()

      for (x in cloned$body)
        x$parent = cloned

      return (cloned)
    },

    set_body = function(value) {
      for (v in value)
        v$parent = self

      self$body = value
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
    condition = NULL,
    true = NULL,
    false = NULL,

    initialize = function(condition, true, false = NULL, parent = NULL) {
      super$initialize(parent)

      self$set_condition(condition)
      self$set_true(true)
      self$set_false(false)
    },

    set_condition = function(value) {
      value$parent = self
      self$condition = value
    },

    set_true = function(value) {
      value$parent = self
      self$true = value
    },

    set_false = function(value) {
      if (!is.null(value))
        value$parent = self
      self$false = value
    }
  )
)

#' @export
For = R6::R6Class("For", inherit = ASTNode,
  "public" = list(
    ivar = NULL,
    iter = NULL,
    body = NULL,

    initialize = function(ivar, iter, body, parent = NULL) {
      super$initialize(parent)

      self$set_ivar(ivar)
      self$set_iter(iter)
      self$set_body(body)
    },

    set_ivar = function(value) {
      value$parent = self
      self$ivar = value
    },

    set_iter = function(value) {
      value$parent = self
      self$iter = value
    },

    set_body = function(value) {
      value$parent = self
      self$body = value
    }
  )
)

#' @export
While = R6::R6Class("While", inherit = ASTNode,
  "public" = list(
    condition = NULL,
    body = NULL,
    is_repeat = FALSE,

    initialize = function(condition, body, is_repeat = FALSE, parent = NULL) {
      super$initialize(parent)

      self$set_condition(condition)
      self$set_body(body)
      self$is_repeat = is_repeat
    },

    set_condition = function(value) {
      value$parent = self
      self$condition = value
    },

    set_body = function(value) {
      value$parent = self
      self$body = value
    }
  )
)


# Calls
# --------------------

#' export
Application = R6::R6Class("Application", inherit = ASTNode,
  "private" = list(
    deep_clone = function(name, value) {
      switch(name,
        "args" = lapply(value, function(v) v$copy())
        , super$deep_clone(name, value)
      )
    }
  ),

  "public" = list(
    args = NULL,

    initialize = function(args = list(), parent = NULL) {
      super$initialize(parent)

      self$set_args(args)
    },

    copy = function() {
      cloned = super$copy()

      for (x in cloned$args)
        x$parent = cloned

      return (cloned)
    },

    set_args = function(value) {
      for (v in value)
        v$parent = self
      self$args = value
    }
  )
)

#' @export
Return = R6::R6Class("Return", inherit = Application,
  "public" = list(
    is_invisible = FALSE,
    initialize = function(args = list(), is_invisible = FALSE, parent = NULL) {
      if (!is.list(args))
        args = list(args)

      super$initialize(args, parent)

      self$is_invisible = is_invisible
    }
  )
)

#' @export
Call = R6::R6Class("Call", inherit = Application,
  "public" = list(
    fn = NULL,

    initialize = function(fn, args = list(), parent = NULL) {
      super$initialize(args, parent)

      self$set_fn(fn)
    },

    set_fn = function(value) {
      # NOTE: fn could be a Symbol, Function, Primitive, or Call.
      if (!inherits(value, "ASTNode"))
        value = Symbol$new(value)

      value$parent = self
      self$fn = value
    }
  ),

)

#' @export
Internal = R6::R6Class("Internal", inherit = Call,
  "public" = list(
    initialize = function(args = NULL, parent = NULL) {
      super$initialize(".Internal", args, parent)
    }
  )
)



# Functions
# --------------------

#' @export
Callable = R6::R6Class("Callable", inherit = ASTNode,
  "private" = list(
    deep_clone = function(name, value) {
      switch(name,
        "params" = lapply(value, function(v) v$copy())
        , super$deep_clone(name, value)
      )
    }
  ),

  "public" = list(
    params = NULL,

    initialize = function(params, parent = NULL) {
      super$initialize(parent)

      self$set_params(params)
    },

    copy = function() {
      cloned = super$copy()

      for (x in cloned$params)
        x$parent = cloned

      return (cloned)
    },

    set_params = function(value) {
      for (v in value)
        v$parent = self
      self$params = value
    }
  )
)

#' @export
Function = R6::R6Class("Function", inherit = Callable,
  "public" = list(
    # FIXME: Need to override $copy() here?
    body = NULL,

    initialize = function(params, body, parent = NULL) {
      super$initialize(params, parent)

      self$set_body(body)
    },

    set_body = function(value) {
      value$parent = self
      self$body = value
    }
  )
)

#' @export
Primitive = R6::R6Class("Primitive", inherit = Callable,
  "public" = list(
    fn = NULL,

    initialize = function(params, fn, parent = NULL) {
      super$initialize(params, parent)

      self$set_fn(fn)
    },

    set_fn = function(value) {
      if (!inherits(value, "Symbol"))
        value = Symbol$new(value)

      value$parent = self
      self$fn = value
    }
  )
)



# Assignment
# --------------------

#' @export
Assign = R6::R6Class("Assign", inherit = ASTNode,
  "public" = list(
    write = NULL,
    read = NULL,

    initialize = function(write, read, parent = NULL) {
      super$initialize(parent)

      self$set_write(write)
      self$set_read(read)
    },

    set_write = function(value) {
      value$parent = self
      self$write = value
    },

    set_read = function(value) {
      value$parent = self
      self$read = value
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
    write = NULL,
    blocks = integer(0),
    read = list(),

    initialize = function(write, parent = NULL) {
      super$initialize(parent)

      self$set_write(write)
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
    },

    set_write = function(value) {
      if (!inherits(value, "Symbol"))
        value = Symbol$new(value)

      value$parent = self
      self$write = value
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

    initialize = function(basename, ssa_number = NA_integer_, parent = NULL) {
      if ( !(is.character(basename) || is.symbol(basename)) )
        stop("Symbol basename must be a character or a name.", call. = FALSE)

      super$initialize(parent)
      self$basename = as.character(basename)
      self$ssa_number = ssa_number
    }
  ),

  "active" = list(
    name = function() {
      ssa_number = self$ssa_number
      if (is.na(ssa_number))
        return (self$basename)

      return (sprintf("%s_%i", self$basename, ssa_number))
    }
  )
)

#' @export
Parameter = R6::R6Class("Parameter", inherit = Symbol,
  "public" = list(
    default = NULL,

    initialize = function(name, default = NULL, ssa = NA_integer_,
      parent = NULL)
    {
      super$initialize(name, ssa, parent)

      self$set_default(default)
    },

    set_default = function(value) {
      if (!is.null(value))
        value$parent = self
      self$default = value
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
