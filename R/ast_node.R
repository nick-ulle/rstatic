
# Classes that represent AST nodes.

#' @export
ASTNode = R6::R6Class("ASTNode",
  public = list(
    parent = NULL,
    initialize = function(parent) {
      self$parent = parent
    }
  )
)

#' @export
Next = R6::R6Class("Next", inherit = ASTNode)

#' @export
Break = R6::R6Class("Break", inherit = ASTNode)

#' @export
If = R6::R6Class("If",
  inherit = ASTNode,
  public = list(
    predicate = NULL,
    true = NULL,
    false = NULL,
    initialize = function(parent, predicate = NULL, true = NULL, false = NULL)
    { 
      super$initialize(parent)
      self$predicate = predicate
      self$true = true
      self$false = false
    }
  )
)

#' @export
For = R6::R6Class("For",
  inherit = ASTNode,
  public = list(
    ivar = NULL,
    iter = NULL,
    body = NULL,
    initialize = function(parent, ivar = NULL, iter = NULL, body = NULL) {
      super$initialize(parent)
      self$ivar = ivar
      self$iter = iter
      self$body = body
    }
  )
)

#' @export
While = R6::R6Class("While",
  inherit = ASTNode,
  public = list(
    predicate = NULL,
    body = NULL,
    is_repeat = FALSE,
    initialize = function(parent, predicate = NULL, body = NULL,
      is_repeat = FALSE) {

      super$initialize(parent)
      self$predicate = predicate
      self$body = body
      self$is_repeat = is_repeat
    }
  )
)

#' @export
Assign = R6::R6Class("Assign",
  inherit = ASTNode,
  public = list(
    write = NULL,
    read = NULL,
    initialize = function(parent, write = NULL, read = NULL) {
      super$initialize(parent)
      self$write = write
      self$read = read
    }
  )
)

#' @export
Call = R6::R6Class("Call",
  inherit = ASTNode,
  public = list(
    name = NULL,
    args = NULL,
    initialize = function(parent, name, args = NULL) {
      super$initialize(parent)
      self$name = name
      self$args = args
    }
  )
)

#' @export
Return = R6::R6Class("Return",
  inherit = Call,
  public = list(
    is_invisible = FALSE,
    initialize = function(parent, args = NULL, is_invisible = FALSE) {
      super$initialize(parent, "return", args)
      self$is_invisible = is_invisible
      if (self$is_invisible)
        self$name = "invisible"
      else
        self$name = "return"
    }
  )
)


#' @export
Internal = R6::R6Class("Internal",
  inherit = Call,
  public = list(
    initialize = function(parent, args = NULL) {
      super$initialize(parent, ".Internal", args)
    }
  )
)


#' @export
Symbol = R6::R6Class("Symbol",
  inherit = ASTNode,
  public = list(
    name = NULL,
    type = NULL,
    initialize = function(parent, name, type = NULL) {
      super$initialize(parent)
      self$name = name
      self$type = type
    }
  )
)

#' @export
Parameter = R6::R6Class("Parameter",
  inherit = Symbol,
  public = list(
    default = NULL,
    initialize = function(parent, name, default = NULL, type = NULL) {
      super$initialize(parent, name, type)
      self$default = default
    }
  )
)

#' @export
Function = R6::R6Class("Function",
  inherit = ASTNode,
  public = list(
    params = NULL,
    body = NULL,
    initialize = function(parent, params = NULL, body = NULL) {
      super$initialize(parent)
      self$params = params
      self$body = body
    }
  )
)

#' @export
Bracket = R6::R6Class("Bracket",
  inherit = ASTNode,
  public = list(
    body = NULL,
    initialize = function(parent, body = list()) {
      super$initialize(parent)
      self$body = body
    }
  )
)

#' @export
Paren = R6::R6Class("Paren",
  inherit = ASTNode,
  public = list(
    body = NULL,
    initialize = function(parent, body = NULL) {
      super$initialize(parent)
      self$body = body
    }
  )
)

#' @export
Literal = R6::R6Class("Literal",
  inherit = ASTNode,
  public = list(
    value = NULL,
    initialize = function(parent, value) {
      super$initialize(parent)
      self$value = value
    }
  )
)

#' @export
Null = R6::R6Class("Null",
  inherit = Literal,
  public = list(
    initialize = function(parent) {
      super$initialize(parent, NULL)
    }
  )
)

#' @export
Logical = R6::R6Class("Logical",
  inherit = Literal
)

#' @export
Integer = R6::R6Class("Integer",
  inherit = Literal
)

#' @export
Numeric = R6::R6Class("Numeric",
  inherit = Literal
)

#' @export
Complex = R6::R6Class("Complex",
  inherit = Literal
)

#' @export
Character = R6::R6Class("Character",
  inherit = Literal
)
