#
# Methods for converting to trees of ASTNode objects.
#

#' @include ast_node.R
NULL


#' Convert Unquoted R Code to ASTNodes
#'
#' This function uses non-standard evaluation to convert unquoted R code to a
#' tree of ASTNode objects.
#'
#' @param expr unquoted R code to be converted.
#' @export
to_astq = function(expr, parent = NULL) {
  to_ast(substitute(expr), parent)
}


#' Convert Quoted R Code to ASTNodes
#'
#' This function converts quoted R code to a tree of ASTNode objects.
#'
#' @param expr (language) quoted R code to be converted.
#' @export
to_ast = function(expr, parent = NULL) {
  UseMethod("to_ast")
}


#' @export
to_ast.function = function(expr, parent = NULL) {
  # Construct a call for the function definition.
  # FIXME: The function might be a closure.
  if (is.primitive(expr)) {
    name = as.character(substitute(expr))
    params = formals(args(sum))
    node = Primitive$new(parent, params, name)
    return (node)
  }

  expr = call("function", formals(expr), body(expr))
  to_ast(expr, parent)
}


#' @export
to_ast.if    = function(expr, parent = NULL) {
  node = If$new(parent)
  node$predicate = to_ast(expr[[2]], node)
  node$true  = to_ast(expr[[3]], node)
  node$false = if (length(expr) == 4) to_ast(expr[[4]], node)
               else NULL

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
  node$body = to_ast(expr[[3]], node)
  return (node)
}

#' Convert a repeat to an ASTNode
#'
to_ast_repeat = function(expr, parent = NULL) {
  node = While$new(parent, predicate = TRUE, is_repeat = TRUE)
  node$body = to_ast(expr[[2]], node)
  return (node)
}


#' @export
`to_ast.=` = function(expr, parent = NULL) {
  read = expr[[3]]
  write = expr[[2]]

  if (inherits(write, "call")) {
    node = Replacement$new(parent, paste0(write[[1]], "<-"))
    read = to_ast(read, node)
    write = lapply(write[-1], to_ast, node)
    # FIXME: the read argument is for the "value" parameter.
    node$args = append(write, read)

  } else {
    node = Assign$new(parent)
    node$read = to_ast(read, node)
    node$write = to_ast(write, node)
  }

  return (node)
}

#' @export
`to_ast.<-` = `to_ast.=`


#' @export
to_ast.call = function(expr, parent = NULL) {
  # Handle calls to anonymous functions.
  func = expr[[1]]
  if (inherits(func, "name")) {
    name = as.character(func)

    # Handle "calls" that don't use the standard call syntax. Most of these are
    # actually keywords.
    if (name == "function")
      return (to_ast_function_def(expr, parent))
    else if (name == "repeat")
      return (to_ast_repeat(expr, parent))
    else if (name == "break")
      return (Break$new(parent))
    else if (name == "next")
      return (Next$new(parent))

    # The standard call syntax applies, so construct an appropriate node.
    node =
      if (name == "return") {
        Return$new(parent)
      } else if (name == "invisible") {
        Return$new(parent, is_invisible = TRUE)
      } else if (name == ".Internal") {
        # TODO: .C .Fortran .Call .External
        Internal$new(parent)
      } else {
        Call$new(parent, name)
      }
  
  } else {
    node = Call$new(parent)
    node$func = to_ast(func, node)
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
  node = Brace$new(parent)
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
