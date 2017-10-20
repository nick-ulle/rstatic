#
# Methods for converting to trees of ASTNode objects.
#


#' Convert Unquoted R Code to ASTNodes
#'
#' This function uses non-standard evaluation to convert unquoted R code to a
#' tree of ASTNode objects.
#'
#' @param expr Unquoted R code to be converted.
#' @export
to_astq = function(expr) {
  to_ast(substitute(expr))
}


#' Convert Quoted R Code to ASTNodes
#'
#' This function converts quoted R code to a tree of ASTNode objects.
#'
#' @param expr (language) Quoted R code to be converted.
#' @export
to_ast = function(expr, insertReturn = TRUE, ...) {
  UseMethod("to_ast")
}


#' @export
to_ast.function = function(expr, insertReturn = TRUE, ...)
{
  name = as.character(substitute(expr))
    
  if(insertReturn)
     expr = insertReturn(expr)
  
  # FIXME: Save the parent environment of the function.
  fn = list(name, formals(args(expr)), body(expr))

  to_ast_callable(fn, is.primitive(expr))
}


#' Convert a Callable Object to an ASTNode
#'
#' This function converts primitives, functions, and function definitions to
#' ASTNode objects. No distinction is made between functions and function
#' definitions since they have the same underlying code.
#'
#' @param expr (language) Quoted R code to be converted.
#' @param is_primitive (logical) Whether or not the expression is a primitive.
#'
to_ast_callable = function(expr, is_primitive = FALSE) {
  # TODO: If this is a function definition, the environment it will exist in
  # hasn't been created yet, so what should happen?
  params = Map(function(name, default) {
    if (inherits(default, "name") && default == "")
      default = NULL
    else
      default = to_ast(default)

    Parameter$new(name, default)
  }, names(expr[[2]]), expr[[2]])

  if (is_primitive) {
    # Construct primitive with params and name.
    Primitive$new(params, expr[[1]])

  } else {
    # Construct function with params and body.
    Function$new(params, to_ast(expr[[3]]))
  }
}



#' @export
to_ast.if = function(expr, insertReturn = TRUE, ...) {
  If$new(
    to_ast(expr[[2]]),
    to_ast(expr[[3]]), 
    if (length(expr) == 4) to_ast(expr[[4]])
    else NULL
  )
}

#' @export
to_ast.for = function(expr, insertReturn = TRUE, ...) {
  For$new(to_ast(expr[[2]]), to_ast(expr[[3]]), to_ast(expr[[4]]))
}

#' @export
to_ast.while = function(expr, insertReturn = TRUE, ...) {
  While$new(to_ast(expr[[2]]), to_ast(expr[[3]]))
}

#' Convert a repeat to an ASTNode
#'
#' @param expr (language) Quoted R code to be converted.
#'
to_ast_repeat = function(expr, insertReturn = TRUE, ...) {
  While$new(Logical$new(TRUE), to_ast(expr[[2]]), is_repeat = TRUE)
}


#' @export
`to_ast.=` = function(expr, insertReturn = TRUE, ...) {
  read = expr[[3]]
  write = expr[[2]]

  if (inherits(write, "call")) {
    # FIXME: the read argument is for the "value" parameter.
    args = append(lapply(write[-1], to_ast), to_ast(read))
    node = Replacement$new(write = args[[1]]$copy(), fn = write[[1]], args)

  } else {
    # FIXME: Eval read before write?
    node = Assign$new(to_ast(write), to_ast(read))
  }

  return (node)
}

#' @export
`to_ast.<-` = `to_ast.=`


#' @export
to_ast.call = function(expr, insertReturn = TRUE, ...) {
  func = expr[[1]]
  if (inherits(func, "name")) {
    name = as.character(func)

    # Handle "calls" that don't use the standard call syntax. Most of these are
    # actually keywords.
    if (name == "function")
      return (to_ast_callable(expr))
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
    # Handle calls to anonymous functions.
    node = Call$new(to_ast(func))
  }

  node$set_args( lapply(expr[-1], to_ast) )
  return (node)
}


#' @export
to_ast.name = function(expr, insertReturn = TRUE, ...) {
  Symbol$new(as.character(expr))
}


#' @export
`to_ast.{` = function(expr, insertReturn = TRUE, ...) {
  Brace$new(lapply(expr[-1], to_ast))
}


#' @export
`to_ast.(` = function(expr, insertReturn = TRUE, ...) {
  Brace$new(lapply(expr[-1], to_ast), is_paren = TRUE)
}


#' @export
to_ast.NULL      = function(expr, insertReturn = TRUE, ...) Null$new()
#' @export
to_ast.logical   = function(expr, insertReturn = TRUE, ...) Logical$new(expr)
#' @export
to_ast.integer   = function(expr, insertReturn = TRUE, ...) Integer$new(expr)
#' @export
to_ast.numeric   = function(expr, insertReturn = TRUE, ...) Numeric$new(expr)
#' @export
to_ast.complex   = function(expr, insertReturn = TRUE, ...) Complex$new(expr)
#' @export
to_ast.character = function(expr, insertReturn = TRUE, ...) Character$new(expr)

#' @export
to_ast.expression = function(expr, insertReturn = TRUE, ...) Brace$new(lapply(expr, to_ast, insertReturn = insertReturn, ...), is_paren = FALSE)


#' @export
to_ast.default = function(expr, insertReturn = TRUE, ...) {
  msg = sprintf("Cannot convert '%s' to an ASTNode.", class(expr)[1])
  stop(msg)
}
