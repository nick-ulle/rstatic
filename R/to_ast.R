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
quote_ast = function(expr) {
  to_ast(substitute(expr))
}


#' Convert Quoted R Code to ASTNodes
#'
#' This function converts quoted R code to a tree of ASTNode objects.
#'
#' @param expr (language) Quoted R code to be converted.
#' @export
to_ast = function(expr) {
  UseMethod("to_ast")
}


#' @export
to_ast.expression = function(expr) {
  Brace$new(lapply(expr, to_ast))
}


#' @export
to_ast.function = function(expr)
{
  name = as.character(substitute(expr))
    
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
    if (is(default, "name") && default == "")
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
    body = to_ast(expr[[3]])
    Function$new(params, wrap_brace(body))
  }
}



#' @export
to_ast.if = function(expr) {
  true = to_ast(expr[[3]])
  if (length(expr) == 4) {
    false = to_ast(expr[[4]])
    false = wrap_brace(false)
  } else
    false = Brace$new()

  If$new(to_ast(expr[[2]]), wrap_brace(true), false)
}

#' @export
to_ast.for = function(expr) {
  body = to_ast(expr[[4]])
  For$new(to_ast(expr[[2]]), to_ast(expr[[3]]), wrap_brace(body))
}

#' @export
to_ast.while = function(expr) {
  body = to_ast(expr[[3]])
  While$new(to_ast(expr[[2]]), wrap_brace(body))
}

#' @export
`to_ast.=` = function(expr) {
  read = expr[[3]]
  write = expr[[2]]

  if (is(write, "call")) {
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
to_ast.call = function(expr) {
  func = expr[[1]]
  if (is(func, "name")) {
    name = as.character(func)

    # FIXME: match.call() would be helpful here but at this point there is no
    # scoping information available.

    node = switch(name,
      # Handle "calls" that don't use the standard call syntax. Most of these
      # are actually keywords.
      "function" =
        return (to_ast_callable(expr))
      , "repeat" = {
          body = to_ast(expr[[2]])
          return (While$new(Logical$new(TRUE), body, is_repeat = TRUE))
        }
      , "break" =
        return (Break$new())
      , "next" =
        return (Next$new())
      , "return" = {
        arg = to_ast(expr[[2]])
        return (Return$new(arg))
      }
      , "<<-" = {
        read = to_ast(expr[[3]])
        write = to_ast(expr[[2]])
        return (SuperAssign$new(write, read))
      }

      # The standard call syntax applies, so construct an appropriate node.
      # TODO: .C .Fortran .Call .External
      , ".Internal" = Internal$new()
      # NOTE: These can all be redefined by users.
      , "::" = Namespace$new(name)
      , ":::" = Namespace$new(name)
      , "[" = Subset$new(name)
      , "[[" = Subset$new(name)
      , "$" = Subset$new(name)

      , Call$new(name)
    )

  } else {
    # Handle calls to anonymous functions.
    node = Call$new(to_ast(func))
  }

  node$args = lapply(expr[-1], to_ast)
  return (node)
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
  Parenthesis$new(to_ast(expr[[2]]))
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
