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
to_ast = function(expr, parent = NULL) {
  UseMethod("to_ast")
}


#to_ast.function = function(expr) {
#
#}


#' @export
to_ast.if    = function(expr, parent = NULL) {
  node = If$new(parent)
  node$predicate = to_ast(expr[[2]], node)
  node$true      = to_ast(expr[[3]], node)
  node$false     = to_ast()
  return (node)
}
#' @export
to_ast.for = function(expr, parent = NULL) {
  node = For$new(parent)
  node$ivar = to_ast(expr[[2]], node)
  node$iter = to_ast(expr[[3]], node)
  node$body = to_ast(expr[[4]], node)
  return (node)
}
#' @export
to_ast.while = function(expr, parent = NULL) {
  node = While$new(parent)
  node$predicate = to_ast(expr[[2]], node)
  node$body      = to_ast(expr[[3]], node)
  return (node)
}
#' @export
`to_ast.=` = function(expr, parent = NULL) {
  node = Assign$new(parent)
  node$read  = to_ast(expr[[3]], node)
  node$write = to_ast(expr[[2]], node)
  return (node)
}
#' @export
`to_ast.<-` = `to_ast.=`


#' @export
to_ast.call = function(expr, parent = NULL) {
  name = as.character(expr[[1]])

  # Function definitions are special case.
  if (name == "function")
    return (to_ast_function_def(expr, parent))

  
  # Construct call node.
  node = 
    if (name == "return") {
      Return$new(parent)
    } else if (name == ".Internal") {
      # TODO: .C .Fortran .Call .External
      Internal$new(parent)
    } else {
      Call$new(parent, name)
    }

  node$args = lapply(expr[-1], to_ast, node)
  return (node)
}

#' Convert a function definition to an ASTNode.
#'
to_ast_function_def = function(expr, parent = NULL) {
  # TODO: Assign scope to function.
  node = Function$new(parent)

  node$params = mapply(function(name, default) {
    param = Parameter$new(parent, name)

    param$default =
      if (class(default) == "name" && default == "")
        NULL
      else
        to_ast(default, param)
    
    return (param)
  }, names(expr[[2]]), expr[[2]], SIMPLIFY = FALSE)

  node$body = to_ast(expr[[3]], node)
  return (node)
}


#' @export
to_ast.name = function(expr, parent = NULL) {
  Symbol$new(parent, as.character(expr))
}


#' @export
`to_ast.{` = function(expr, parent = NULL) {
  node = Bracket$new(parent)
  node$body = lapply(expr[-1], to_ast, node)
  return (node)
}


#' @export
`to_ast.(` = function(expr, parent) {
  node = Paren$new(parent)
  node$body = to_ast(expr[[2]], node)
  return (node)
}


#' @export
to_ast.NULL      = function(expr, parent = NULL) Null$new(parent)
#' @export
to_ast.logical   = function(expr, parent = NULL) Logical$new(parent, expr)
#' @export
to_ast.integer   = function(expr, parent = NULL) Integer$new(parent, expr)
#' @export
to_ast.numeric   = function(expr, parent = NULL) Numeric$new(parent, expr)
#' @export
to_ast.complex   = function(expr, parent = NULL) Complex$new(parent, expr)
#' @export
to_ast.character = function(expr, parent = NULL) Character$new(parent, expr)


#' @export
to_ast.default = function(expr, parent = NULL) {
  msg = sprintf("Cannot convert '%s' to an ASTNode.", class(expr)[1])
  stop(msg)
}
