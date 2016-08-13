#
# Methods for converting to trees of ASTNodes.
#

#' @include ast_node.R
NULL


#' Convert R Code to ASTNodes
#'
#' This function converts quoted R code to the corresponding tree of ASTNodes.
#'
#' @param expr (language) The R code to be converted.
#' @export
to_ast = function(expr) {
  UseMethod("to_ast")
}


#to_ast.function = function(expr) {
#
#}


#' @export
to_ast.if    = function(expr) do.call(If$new, lapply(expr[-1], to_ast))
#' @export
to_ast.for   = function(expr) do.call(For$new, lapply(expr[-1], to_ast))
#' @export
to_ast.while = function(expr) do.call(While$new, lapply(expr[-1], to_ast))
#' @export
`to_ast.=`   = function(expr) do.call(Assign$new, lapply(expr[-1], to_ast))
#' @export
`to_ast.<-`  = `to_ast.=`


#' @export
to_ast.call = function(expr) {
  name = as.character(expr[[1]])

  # Function definitions are special case.
  if (name == "function")
    return (to_ast_function_def(expr))

  args = lapply(expr[-1], to_ast)
  
  # Construct call node.
  if (name == "return") {
    Return$new(args)
  } else if (name == ".Internal") {
    Internal$new(args)
  } else {
    # TODO: .C .Fortran .Call .External
    Call$new(name, args)
  }
}

#' Convert a function definition to an ASTNode.
#'
to_ast_function_def = function(expr) {
  params = mapply(function(name, default) {
    if (class(default) == "name" && default == "")
      default = NULL
    else
      default = to_ast(default)
    
    Parameter$new(name, default)
  }, names(expr[[2]]), expr[[2]], SIMPLIFY = FALSE)

  body = to_ast(expr[[3]])

  # TODO: Assign scope for each parameter with Symbol default.
  Function$new(params, body)
}


#' @export
to_ast.name = function(expr) {
  Symbol$new(as.character(expr))
}


#' @export
`to_ast.{` = function(expr) {
  Bracket$new(lapply(expr[-1], to_ast))
}


#' @export
`to_ast.(` = function(expr) {
  Paren$new(to_ast(expr[[2]]))
}


#' @export
to_ast.NULL      = function(expr) Null$new(expr)
#' @export
to_ast.logical   = function(expr) Logical$new(expr)
#' @export
to_ast.integer   = function(expr) Integer$new(expr)
#' @export
to_ast.numeric   = function(expr) Numeric$new(expr)
#' @export
to_ast.complex   = function(expr) Complex$new(expr)
#' @export
to_ast.character = function(expr) Character$new(expr)


#' @export
to_ast.default = function(expr) {
  msg = sprintf("Cannot convert '%s' to an ASTNode.", class(expr)[1])
  stop(msg)
}
