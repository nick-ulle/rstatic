#
# Methods for converting to trees of ASTNode objects.
#


#' Convert Unquoted R Code to ASTNodes
#'
#' This function uses non-standard evaluation to convert unquoted R code to a
#' tree of ASTNode objects.
#'
#' @param expr unquoted R code to be converted.
#' @export
to_astq = function(expr) {
  to_ast(substitute(expr))
}


#' Convert Quoted R Code to ASTNodes
#'
#' This function converts quoted R code to a tree of ASTNode objects.
#'
#' @param expr (language) quoted R code to be converted.
#' @export
to_ast = function(expr) {
  UseMethod("to_ast")
}


#' @export
to_ast.function = function(expr) {
  # Construct a call for the function definition.
  # FIXME: The function might be a closure.
  if (is.primitive(expr)) {
    name = as.character(substitute(expr))
    params = formals(args(sum))
    node = Primitive$new(params, name)
    return (node)
  }

  expr = call("function", formals(expr), body(expr))
  to_ast(expr)
}


#' @export
to_ast.if = function(expr) {
  If$new(
    to_ast(expr[[2]]),
    to_ast(expr[[3]]), 
    if (length(expr) == 4) to_ast(expr[[4]])
    else NULL
  )
}

#' @export
to_ast.for = function(expr) {
  For$new(to_ast(expr[[2]]), to_ast(expr[[3]]), to_ast(expr[[4]]))
}

#' @export
to_ast.while = function(expr) {
  While$new(to_ast(expr[[2]]), to_ast(expr[[3]]))
}

#' Convert a repeat to an ASTNode
#'
to_ast_repeat = function(expr) {
  While$new(Logical$new(TRUE), to_ast(expr[[2]]), is_repeat = TRUE)
}


#' @export
`to_ast.=` = function(expr) {
  read = expr[[3]]
  write = expr[[2]]

  if (inherits(write, "call")) {
    # FIXME: the read argument is for the "value" parameter.
    args = append(lapply(write[-1], to_ast), to_ast(read))
    node = Replacement$new(write[[1]], args)

  } else {
    # FIXME: Eval read before write?
    node = Assign$new(to_ast(write), to_ast(read))
  }

  return (node)
}

#' @export
`to_ast.<-` = `to_ast.=`


#' @export
to_ast.call = function(expr) {
  # Handle calls to anonymous functions.
  func = expr[[1]]
  if (inherits(func, "name")) {
    name = as.character(func)

    # Handle "calls" that don't use the standard call syntax. Most of these are
    # actually keywords.
    if (name == "function")
      return (to_ast_function_def(expr))
    else if (name == "repeat")
      return (to_ast_repeat(expr))
    else if (name == "break")
      return (Break$new())
    else if (name == "next")
      return (Next$new())

    # The standard call syntax applies, so construct an appropriate node.
    node = switch(name,
      "return"      = Return$new()
      , "invisible" = Return$new(is_invisible = TRUE)
      # TODO: .C .Fortran .Call .External
      , ".Internal" = Internal$new()
      , Call$new(name)
    )
  } else {
    node = Call$new(to_ast(func))
  }

  node$args = lapply(expr[-1], to_ast)
  return (node)
}

# FIXME:
#' Convert a function definition to an ASTNode.
#'
to_ast_function_def = function(expr) {
  # TODO: Assign scope to function.
  params = mapply(function(name, default) {
    Parameter$new(
      name,
      if (class(default) == "name" && default == "") NULL
      else to_ast(default)
    )
  }, names(expr[[2]]), expr[[2]], SIMPLIFY = FALSE)

  Function$new(params, to_ast(expr[[3]]))
}


#' @export
to_ast.name = function(expr) {
  Symbol$new(as.character(expr))
}


#' @export
`to_ast.{` = function(expr) {
  Brace$new(lapply(expr[-1], to_ast))
}


#' @export
`to_ast.(` = function(expr) {
  Brace$new(lapply(expr[-1], to_ast), is_paren = TRUE)
}


#' @export
to_ast.NULL      = function(expr) Null$new()
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
