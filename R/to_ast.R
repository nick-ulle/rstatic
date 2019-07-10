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
to_ast.default =
function(expr)
{
  if (is.function(expr)) {
    to_ast.function(expr)

  } else {
    msg = sprintf(
      "no applicable method for 'to_ast' applied to an object of class %s"
      , paste0("'", class(expr), "'", collpase = ", "))
    stop(msg)
  }
}


#' List Formal Arguments
#'
#' This function returns the list of formal arguments, also called parameters,
#' for a function object.
#'
#' For primitives that have no named parameters, this function returns an empty
#' list.
#'
list_formals = function(expr) {
  if (is.primitive(expr)) {
    # formals() doesn't support primitives, so use args() first to get a
    # function object.
    expr = args(expr)

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
    if (is.null(expr))
      return (list())
  }

  as.list(formals(expr))
}


#' @export
to_ast.function =
function(expr)
{
  name = substitute(expr)
  # Anonymous functions are a "call" instead of a "symbol". They don't have a
  # name.
  if (!is.name(name))
    name = NULL

  param_list = list_formals(expr)

  # FIXME: Save the parent environment of the function.
  fn = list(name, param_list, body(expr))
  to_ast_callable(fn, is.primitive(expr))
}

#' @export
to_ast.ASTNode =
function(expr)
{
  expr
}

to_ast_parameters =
function(params) {
  is_param = vapply(params, is, NA, "Parameter")
  if (all(is_param))
    return(params)

  names = names(params)
  if (is.null(names))
    stop("All parameters must have names.")

  params[!is_param] = mapply(
    function(name, default) {
      Parameter$new(name, to_ast(default))
    }
    , names[!is_param], params[!is_param]
    , SIMPLIFY = FALSE)

  params
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
  #params = to_ast_parameters(expr[[2]])

  # A function object is a `function`, and second element is a list.
  # A function definition is a `call`, and second element may be NULL.
  # So use as.list() here to make sure params is a list.
  params = as.list(expr[[2]])

  if (is_primitive) {
    # Construct primitive with params and name.
    Primitive$new(expr[[1]], params)

  } else {
    # Construct function with params and body.
    body = to_ast(expr[[3]])
    Function$new(wrap_brace(body), params)
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
    name = paste0(write[[1]], "<-")
    fn = Symbol$new(name)
    # NOTE: `read` is the `value` argument to the replacement function.
    args = c(lapply(write[-1], to_ast), read)

    write = copy(args[[1L]])
    switch(name
      , "[<-" = Replacement1$new
      , "[[<-" = Replacement2$new
      , "$<-" = ReplacementDollar$new
      , Replacement$new
    )(write, Call$new(fn, args))

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

    node = switch(name
      # Handle "calls" that don't use the standard call syntax. Most of these
      # are actually keywords.
      , "function" =
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
      , "[" = Subset1$new(name)
      , "[[" = Subset2$new(name)
      , "$" = SubsetDollar$new(name)

      , Call$new(name)
    )

  } else {
    # Handle calls to anonymous functions.
    node = Call$new(to_ast(func))
  }

  node$args = ArgumentList$new(lapply(expr[-1], to_ast))
  return (node)
}


#' @export
to_ast.name = function(expr) {
  name = as.character(expr)
  if(nzchar(name))
    Symbol$new(name)
  else
    EmptyArgument$new()
}

#' @export
to_ast.missing = function(expr) {
  EmptyArgument$new()
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
