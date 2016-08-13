
# Classes that represent AST nodes.

#' @export
ASTNode = R6::R6Class("ASTNode",
  public = list(
    parent = NULL
  )
)

#' @export
If = R6::R6Class("If",
  inherit = ASTNode,
  public = list(
    predicate = NULL,
    true = NULL,
    false = NULL,
    initialize = function(predicate, true, false = NULL) {
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
    initialize = function(ivar, iter, body) {
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
    initialize = function(predicate, body) {
      self$predicate = predicate
      self$body = body
    }
  )
)

#' @export
Assign = R6::R6Class("Assign",
  inherit = ASTNode,
  public = list(
    left = NULL,
    right = NULL,
    initialize = function(left, right) {
      self$left = left
      self$right = right
    }
  )
)

#' @export
Call = R6::R6Class("Call",
  inherit = ASTNode,
  public = list(
    name = NULL,
    args = NULL,
    initialize = function(name, args) {
      self$name = name
      self$args = args
    }
  )
)

#' @export
Return = R6::R6Class("Return",
  inherit = Call,
  public = list(
    name = "return",
    initialize = function(args) {
      self$args = args
    }
  )
)


#' @export
Internal = R6::R6Class("Internal",
  inherit = Call,
  public = list(
    name = ".Internal",
    initialize = function(args) {
      self$args = args
    }
  )
)


#' @export
Symbol = R6::R6Class("Symbol",
  inherit = ASTNode,
  public = list(
    name = NULL,
    type = NULL,
    initialize = function(name, type = NULL) {
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
    initialize = function(name, default, type = NULL) {
      self$name = name
      self$default = default
      self$type = type
    }
  )
)

#' @export
Function = R6::R6Class("Function",
  inherit = ASTNode,
  public = list(
    params = NULL,
    body = NULL,
    initialize = function(params, body) {
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
    initialize = function(body = list()) {
      self$body = body
    }
  )
)

#' @export
Paren = R6::R6Class("Paren",
  inherit = ASTNode,
  public = list(
    body = NULL,
    initialize = function(body = NULL) {
      self$body = body
    }
  )
)

#' @export
Literal = R6::R6Class("Literal",
  inherit = ASTNode,
  public = list(
    value = NULL,
    type = NULL,
    initialize = function(value, type = NULL) {
      self$value = value
      self$type = type
    }
  )
)

#' @export
Null = R6::R6Class("Null",
  inherit = Literal,
  public = list(
    initialize = function(value = NULL, type = NULL) {
      self$value = NULL
      self$type = type
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
