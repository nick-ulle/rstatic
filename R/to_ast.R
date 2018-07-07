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
  Brace$new(lapply(expr, to_ast), is_hidden = TRUE)
}


#' @export
to_ast.default = function(expr) {
  if (is.function(expr)) {
    to_ast.function(expr)

  } else {
    msg = sprintf(
      "no applicable method for 'to_ast' applied to an object of class %s"
      , paste0("'", class(expr), "'", collpase = ", "))
    stop(msg)
  }
}


#' @export
to_ast.function = function(expr)
{
  name = as.character(substitute(expr))
  arg_list = args(expr)

  # FIXME: The following primitives have no named parameters, so `args()`
  # returns `NULL`.
  #
  #  [1] ":"        "("        "["        "[["       "[[<-"     "[<-"
  #  [7] "{"        "@"        "@<-"      "&&"       "<-"       "<<-"
  # [13] "="        "||"       "~"        "$"        "$<-"      "break"
  # [19] "for"      "function" "if"       "next"     "repeat"   "return"
  # [25] "while"
  #
  # For now, treat these like they do not have any parameters.
  if (is.null(arg_list))
    arg_list = list()
  else
    arg_list = formals(arg_list)

  # FIXME: Save the parent environment of the function.
  fn = list(name, arg_list, body(expr))
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
  params = Map(function(name, default) {
    Parameter$new(name, to_ast(default))
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
    false = Brace$new(is_hidden = TRUE)

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
  read = to_ast(expr[[3]])
  write = expr[[2]]

  if (is.call(write)) {
    fn = Symbol$new(paste0(write[[1]], "<-"))
    # NOTE: `read` is the `value` argument to the replacement function.
    args = c(lapply(write[-1], to_ast), read)

    Replacement$new(args[[1]]$copy(), Call$new(fn, args))

  } else {
    Assign$new(to_ast(write), read)
  }
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
          if(length(expr) > 1)
              arg = to_ast(expr[[2]])
          else
              arg = Null$new()
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
  if(nzchar(expr))
    Symbol$new(name)
  else
    Missing$new()
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



#' Wrap ASTNode With Brace
#'
#' This function wraps an ASTNode in a Brace if it isn't one already.
#'
#' @export
wrap_brace = function(node) {
  if (is(node, "Container"))
    node
  else
    Brace$new(node, is_hidden = TRUE)
}
