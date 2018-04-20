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
        .copy_ast(value)
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
        if (bindingIsActive(name, cloned))
          next

        item = get(name, cloned)
        if (is.function(item))
          next

        item = .reparent_ast(item, cloned)
        assign(name, item, envir = cloned)
      }

      cloned
    }
  )
)


# Containers
# --------------------
#' @export
Container = R6::R6Class("Container", inherit = ASTNode,
  "public" = list(
    .body = NULL,

    initialize = function(body = list(), parent = NULL) {
      super$initialize(parent)

      if (!is.list(body))
        body = list(body)

      self$body = body
    }
  ),

  "active" = list(
    body = function(value) {
      if (missing(value))
        return (self$.body)

      self$.body = .reparent_ast(value, self)
    }
  )
)

#' @export
BlockList = R6::R6Class("BlockList", inherit = Container)

#' @export
Brace = R6::R6Class("Brace", inherit = Container,
  "public" = list(
    id = NA_character_,
    .phi = NULL,

    initialize = function(body = list(), id = NA_character_,
      phi = list(), parent = NULL)
    {
      super$initialize(body, parent)

      self$id = id
      self$phi = phi
    },

    set_phi = function(phi) {
      self$.phi[[phi$write$basename]] = .reparent_ast(phi, self)

      invisible(NULL)
    }
  ),

  "active" = list(
    phi = function(value) {
      if (missing(value))
        return (self$.phi)

      self$.phi = .reparent_ast(value, self)
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

      self$.condition = .reparent_ast(value, self)
    },

    true = function(value) {
      if (missing(value))
        return (self$.true)

      value = as_blocks(value)
      self$.true = .reparent_ast(value, self)
    },

    false = function(value) {
      if (missing(value))
        return (self$.false)

      value = as_blocks(value)
      self$.false = .reparent_ast(value, self)
    }
  )
)

#' @export
Loop = R6::R6Class("Loop", inherit = ASTNode,
  "public" = list(
    .test = NULL,
    .body = NULL,

    initialize = function(body, parent = NULL) {
      super$initialize(parent)

      self$body = body
    }
  ),

  "active" = list(
    test = function(value) {
      if (missing(value))
        return (self$.test)

      value = as_blocks(value)
      self$.test = .reparent_ast(value, self)
    },

    body = function(value) {
      if (missing(value))
        return (self$.body)

      value = as_blocks(value)
      self$.body = .reparent_ast(value, self)
    }
  )
)

#' @export
For = R6::R6Class("For", inherit = Loop,
  "public" = list(
    .ivar = NULL,
    .iter = NULL,
    .setup = NULL,
    .increment = NULL,

    initialize = function(ivar, iter, body, parent = NULL) {
      super$initialize(body, parent)

      self$ivar = ivar
      self$iter = iter
    }
  ),

  "active" = list(
    ivar = function(value) {
      if (missing(value))
        return (self$.ivar)

      self$.ivar = .reparent_ast(value, self)
    },

    iter = function(value) {
      if (missing(value))
        return (self$.iter)

      self$.iter = .reparent_ast(value, self)
    },

    setup = function(value) {
      if (missing(value))
        return (self$.setup)

      value = as_blocks(value)
      self$.setup = .reparent_ast(value, self)
    },

    increment = function(value) {
      if (missing(value))
        return (self$.increment)

      value = as_blocks(value)
      self$.increment = .reparent_ast(value, self)
    }
  )
)

#' @export
While = R6::R6Class("While", inherit = Loop,
  "public" = list(
    .condition = NULL,
    is_repeat = FALSE,

    initialize = function(condition, body, is_repeat = FALSE, parent = NULL) {
      super$initialize(body, parent)

      self$condition = condition
      self$is_repeat = is_repeat
    }
  ),

  "active" = list(
    condition = function(value) {
      if (missing(value))
        return (self$.condition)

      self$.condition = .reparent_ast(value, self)
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

      if (!is.list(value))
        value = list(value)
      self$.args = .reparent_ast(value, self)
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
      if (!is(value, "ASTNode"))
        value = Symbol$new(value)

      self$.fn = .reparent_ast(value, self)
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
Parenthesis = R6::R6Class("Parenthesis", inherit = Application)

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

      self$.params = .reparent_ast(value, self)
    }
  )
)

#' @export
Function = R6::R6Class("Function", inherit = Callable,
  "public" = list(
    .body = NULL,
    cfg = NULL,
    ssa = NULL,

    initialize = function(params, body, parent = NULL) {
      super$initialize(params, parent)

      self$body = body
    }
  ),

  "active" = list(
    body = function(value) {
      if (missing(value))
        return (self$.body)

      value = as_blocks(value)
      self$.body = .reparent_ast(value, self)
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

      if (!is(value, "Symbol"))
        value = Symbol$new(value)

      self$.fn = .reparent_ast(value, self)
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

      self$.write = .reparent_ast(value, self)
    },

    read = function(value) {
      if (missing(value))
        return (self$.read)

      self$.read = .reparent_ast(value, self)
    }
  )
)

#' @export
SuperAssign = R6::R6Class("SuperAssign", inherit = Assign)

#' @export
Replacement = R6::R6Class("Replacement", inherit = Assign,
  "public" = list(
    initialize = function(write, fn, args, parent = NULL) {
      if (!is(fn, "ASTNode")) {
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
Return = R6::R6Class("Return", inherit = Assign,
  # NOTE: Return is a subclass of Assign because the CFG models returning `x`
  # as setting the special variable `._return_ <- x` and then branching to the
  # exit block.
  "public" = list(
    initialize = function(args, parent = NULL) {
      write = Symbol$new("._return_")
      super$initialize(write, args, parent)
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

      self$.write = .reparent_ast(value, self)
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

      self$.default = .reparent_ast(value, self)
    }
  )
)


#' @export
Missing = R6::R6Class("Missing", inherit = Symbol,
  "public" = list(
    basename = NULL,
    ssa_number = NULL,
    namespace = NULL,
    namespace_fn = NULL,

    initialize = function(...)
    {
      super$initialize("", ...)
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
.copy_ast         = function(value) UseMethod(".copy_ast")
#' @export
.copy_ast.ASTNode = function(value) value$copy()
#' @export
.copy_ast.list    = function(value) lapply(value, .copy_ast)
#' @export
.copy_ast.R6      = function(value) value$clone(deep = TRUE)
#' @export
.copy_ast.default = function(value) value

.reparent_ast = function(value, parent)
  UseMethod(".reparent_ast")

#' @export
.reparent_ast.ASTNode = function(value, parent) {
  value$parent = parent
  value
}

#' @export
.reparent_ast.list = function(value, parent) {
  lapply(value, .reparent_ast, parent)
}

#' @export
.reparent_ast.default = function(value, parent)
  value
