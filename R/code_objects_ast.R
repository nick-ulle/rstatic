# Factory function to create bindings.
binding_factory = function(field, container = FALSE) {
  binding = function(value) {}

  # Substitute the field name into the function since the function's
  # environment will be reset by R6.
  if (container) {
    body(binding) = substitute({
      if (missing(value))
        return (self$field)

      if (!is.list(value))
        value = list(value)

      self$field = set_parent(value, self)
    }, list(field = field))

  } else {
    body(binding) = substitute({
      if (missing(value))
        return (self$field)

      self$field = set_parent(value, self)
    }, list(field = field))
  }

  binding
}


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
      names = ls(cloned, all.names = TRUE)
      names = setdiff(names, c("parent", ".__enclos_env__"))
      for (name in names) {
        if (bindingIsActive(name, cloned))
          next

        item = get(name, cloned)
        if (is.function(item))
          next

        item = set_parent(item, cloned)
        assign(name, item, envir = cloned)
      }

      cloned
    }
  )
)


# Containers ----------------------------------------

Container = R6::R6Class("Container", inherit = ASTNode,
  "public" = list(
    .contents = NULL,

    initialize = function(contents = list(), parent = NULL) {
      super$initialize(parent)

      self$contents = contents
    }
  ),

  "active" = list(
    contents = binding_factory(".contents", container = TRUE)
  )
)

#' @export
Brace = R6::R6Class("Brace", inherit = Container,
  "public" = list(
    is_hidden = FALSE,

    initialize = function(contents = list(), is_hidden = FALSE, parent = NULL)
    {
      super$initialize(contents = contents, parent = parent)

      self$is_hidden = is_hidden
    }
  )
)


# Control Flow ----------------------------------------

ControlFlow = R6::R6Class("ControlFlow", inherit = ASTNode)

#' @export
Branch = R6::R6Class("Branch", inherit = ControlFlow,
  "public" = list(
    target = NULL,

    initialize = function(target = Label$new(), parent = NULL) {
      super$initialize(parent)

      self$target = target
    }
  )
)

#' @export
Next = R6::R6Class("Next", inherit = Branch)

#' @export
Break = R6::R6Class("Break", inherit = Branch)

#' @export
Return = R6::R6Class("Return", inherit = Branch,
  # NOTE: We model Return as an assignment to the special variable `._return_`
  # followed by a branch to the exit block.
  "public" = list(
    .write = NULL,
    .read = NULL,

    initialize = function(args, parent = NULL) {
      super$initialize(parent = parent)

      self$write = Symbol$new("._return_")
      self$read = args
    }
  ),

  "active" = list(
    write = binding_factory(".write"),
    read  = binding_factory(".read")
  )
)


ConditionalBranch = R6::R6Class("ConditionalBranch", inherit = ControlFlow,
  "public" = list(
    .body = NULL,
    .exit = NULL,

    initialize = function(body, exit = NULL, parent = NULL) {
      super$initialize(parent = parent)

      self$body = body
      self$exit = exit
    }
  ),

  "active" = list(
    body = binding_factory(".body"),
    exit = binding_factory(".exit")
  )
)

#' @export
If = R6::R6Class("If", inherit = ConditionalBranch,
  "public" = list(
    .condition = NULL,

    initialize = function(condition, true, false = Brace$new(),
      parent = NULL)
    {
      super$initialize(body = true, exit = false, parent = parent)

      self$condition = condition
    }
  ),

  "active" = list(
    condition = binding_factory(".condition"),
    # Keep old true/false bindings for interactive use.
    true      = binding_factory(".body"),
    false     = binding_factory(".exit")
  )
)


Loop = R6::R6Class("Loop", inherit = ConditionalBranch)

#' @export
For = R6::R6Class("For", inherit = Loop,
  "public" = list(
    .variable = NULL,
    .iterator = NULL,

    initialize = function(variable, iterator, body, parent = NULL) {
      super$initialize(body = body, parent = parent)

      self$variable = variable
      self$iterator = iterator
    }
  ),

  "active" = list(
    variable = binding_factory(".variable"),
    iterator = binding_factory(".iterator")
  )
)

#' @export
While = R6::R6Class("While", inherit = Loop,
  "public" = list(
    .condition = NULL,
    is_repeat = FALSE,

    initialize = function(condition, body, is_repeat = FALSE, parent = NULL) {
      super$initialize(body = body, parent = parent)

      self$condition = condition
      self$is_repeat = is_repeat
    }
  ),

  "active" = list(
    condition = binding_factory(".condition")
  )
)


# Calls ----------------------------------------

Invocation = R6::R6Class("Invocation", inherit = ASTNode,
  "public" = list(
    .args = NULL,

    initialize = function(args = list(), parent = NULL) {
      super$initialize(parent = parent)

      self$args = args
    }
  ),

  "active" = list(
    args = binding_factory(".args")
  )
)

#' @export
Call = R6::R6Class("Call", inherit = Invocation,
  "public" = list(
    .fn = NULL,

    initialize = function(fn, args = list(), parent = NULL) {
      super$initialize(args = args, parent = parent)

      # NOTE: fn could be a Symbol, Function, Primitive, or Call.
      if (!is(fn, "ASTNode"))
        fn = Symbol$new(fn)

      self$fn = fn
    }
  ),

  "active" = list(
    fn = binding_factory(".fn")
  )
)

#' @export
Internal = R6::R6Class("Internal", inherit = Call,
  "public" = list(
    initialize = function(args = NULL, parent = NULL) {
      super$initialize(fn = ".Internal", args = args, parent = parent)
    }
  )
)

#' @export
Parenthesis = R6::R6Class("Parenthesis", inherit = Invocation)

#' @export
Namespace = R6::R6Class("Namespace", inherit = Call)

#' @export
Subset = R6::R6Class("Subset", inherit = Call)


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
    write = binding_factory(".write"),
    read  = binding_factory(".read")
  )
)

#' @export
SuperAssign = R6::R6Class("SuperAssign", inherit = Assign)

#' @export
Replacement = R6::R6Class("Replacement", inherit = Assign)


# Symbols ----------------------------------------

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

    # FIXME: Maybe default should be 3rd argument.
    initialize = function(basename, default = NULL, ssa_number = NA_integer_,
      parent = NULL)
    {
      super$initialize(basename = basename, ssa_number = ssa_number,
        parent = parent)

      self$default = default
    }
  ),

  "active" = list(
    default = binding_factory(".default")
  )
)



# Functions
# --------------------

Callable = R6::R6Class("Callable", inherit = ASTNode,
  "public" = list(
    .params = NULL,

    initialize = function(params, parent = NULL) {
      super$initialize(parent = parent)

      self$params = params
    }
  ),

  "active" = list(
    params = binding_factory(".params")
  )
)

#' @export
Function = R6::R6Class("Function", inherit = Callable,
  "public" = list(
    .body = NULL,

    initialize = function(params, body, parent = NULL) {
      super$initialize(params = params, parent = parent)

      self$body = body
    }
  ),

  "active" = list(
    body = binding_factory(".body")
  )
)

#' @export
Primitive = R6::R6Class("Primitive", inherit = Callable,
  "public" = list(
    .fn = NULL,

    initialize = function(params, fn, parent = NULL) {
      super$initialize(params = params, parent = parent)

      if (!is(fn, "Symbol"))
        fn = Symbol$new(fn)

      self$fn = fn
    }
  ),

  "active" = list(
    fn = binding_factory(".fn")
  )
)

# Literals ----------------------------------------

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


# Cloning Methods ----------------------------------------

.copy_ast         = function(value) UseMethod(".copy_ast")
#' @export
.copy_ast.ASTNode = function(value) value$copy()
#' @export
.copy_ast.list    = function(value) lapply(value, .copy_ast)
#' @export
.copy_ast.R6      = function(value) value$clone(deep = TRUE)
#' @export
.copy_ast.default = function(value) value


set_parent = function(node, parent)
  UseMethod("set_parent")

#' @export
set_parent.ASTNode = function(node, parent) {
  node$parent = parent
  node
}

#' @export
set_parent.list = function(node, parent) {
  lapply(node, set_parent, parent)
}

#' @export
set_parent.default = function(node, parent)
  node