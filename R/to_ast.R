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
toASTq = function(expr) {
  toAST(substitute(expr))
}


#' Convert Quoted R Code to ASTNodes
#'
#' This function converts quoted R code to a tree of ASTNode objects.
#'
#' @param expr (language) Quoted R code to be converted.
#' @export
toAST = function(expr) {
  UseMethod("toAST")
}


#' @export
toAST.function = function(expr)
{
  name = as.character(substitute(expr))
    
  # FIXME: Save the parent environment of the function.
  fn = list(name, formals(args(expr)), body(expr))

  toASTCallable(fn, is.primitive(expr))
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
toASTCallable = function(expr, is_primitive = FALSE) {
  # TODO: If this is a function definition, the environment it will exist in
  # hasn't been created yet, so what should happen?
  params = Map(function(name, default) {
    if (inherits(default, "name") && default == "")
      default = NULL
    else
      default = toAST(default)

    Parameter$new(name, default)
  }, names(expr[[2]]), expr[[2]])

  if (is_primitive) {
    # Construct primitive with params and name.
    Primitive$new(params, expr[[1]])

  } else {
    # Construct function with params and body.
    Function$new(params, toAST(expr[[3]]))
  }
}



#' @export
toAST.if = function(expr) {
  If$new(
    toAST(expr[[2]]),
    toAST(expr[[3]]), 
    if (length(expr) == 4) toAST(expr[[4]])
    else NULL
  )
}

#' @export
toAST.for = function(expr) {
  For$new(toAST(expr[[2]]), toAST(expr[[3]]), toAST(expr[[4]]))
}

#' @export
toAST.while = function(expr) {
  While$new(toAST(expr[[2]]), toAST(expr[[3]]))
}

#' Convert a repeat to an ASTNode
#'
#' @param expr (language) Quoted R code to be converted.
#'
toASTRepeat = function(expr) {
  While$new(Logical$new(TRUE), toAST(expr[[2]]), is_repeat = TRUE)
}


#' @export
`toAST.=` = function(expr) {
  read = expr[[3]]
  write = expr[[2]]

  if (inherits(write, "call")) {
    # FIXME: the read argument is for the "value" parameter.
    args = append(lapply(write[-1], toAST), toAST(read))
    node = Replacement$new(write = args[[1]]$copy(), fn = write[[1]], args)

  } else {
    # FIXME: Eval read before write?
    node = Assign$new(toAST(write), toAST(read))
  }

  return (node)
}

#' @export
`toAST.<-` = `toAST.=`


#' @export
toAST.call = function(expr) {
  func = expr[[1]]
  if (inherits(func, "name")) {
    name = as.character(func)

    # Handle "calls" that don't use the standard call syntax. Most of these are
    # actually keywords.
    if (name == "function")
      return (toASTCallable(expr))
    else if (name == "repeat")
      return (toASTRepeat(expr))
    else if (name == "break")
      return (Break$new())
    else if (name == "next")
      return (Next$new())

    # The standard call syntax applies, so construct an appropriate node.
    node = switch(name,
      "return"      = Return$new()
      # TODO: .C .Fortran .Call .External
      , ".Internal" = Internal$new()
      , Call$new(name)
    )

  } else {
    # Handle calls to anonymous functions.
    node = Call$new(toAST(func))
  }

  node$args = lapply(expr[-1], toAST)
  return (node)
}


#' @export
toAST.name = function(expr) {
  Symbol$new(as.character(expr))
}


#' @export
`toAST.{` = function(expr) {
  Brace$new(lapply(expr[-1], toAST))
}


#' @export
`toAST.(` = function(expr) {
  Brace$new(lapply(expr[-1], toAST), is_paren = TRUE)
}


#' @export
toAST.NULL      = function(expr) Null$new()
#' @export
toAST.logical   = function(expr) Logical$new(expr)
#' @export
toAST.integer   = function(expr) Integer$new(expr)
#' @export
toAST.numeric   = function(expr) Numeric$new(expr)
#' @export
toAST.complex   = function(expr) Complex$new(expr)
#' @export
toAST.character = function(expr) Character$new(expr)


#' @export
toAST.default = function(expr) {
  msg = sprintf("Cannot convert '%s' to an ASTNode.", class(expr)[1])
  stop(msg)
}
