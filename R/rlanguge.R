# Compatibility with R's language objects.

#' Extract or Replace the Body of a Function Definition
#'
#' This function extracts or replaces the body of the given function
#' definition.
#'
#' @param fun (Function) The function definition from which to extract or
#' replace the body.
#' @param envir Currently unused.
#' @param value The value to replace the body with.
#'
#' @export
body =
function(fun)
{
  UseMethod("body")
}

#' @export
body.Function =
function(fun)
{
  # NOTE: This is actually not compatible with R language objects. Calling
  # `body()` on a quoted function definition returns NULL, with a warning.
  fun$body
}

#' @export
body.ConditionalBranch = body.Function

#' @export
body.default = base::body


#' @rdname body
#' @export
`body<-` =
function(fun, envir, value)
{
  UseMethod("body<-")
}

#' @export
`body<-.Function` =
function(fun, value)
{
  fun$body = value
}

#' @export
`body<-.default` = base::`body<-`


#' Extract or Replace the Parameters of a Function Definition
#'
#' This function extracts or replaces the parameters (formal arguments) of the
#' given function definition.
#'
#' @param fun (Function) The function definition from which to extract or
#' replace the parameters.
#' @param envir Currently unused.
#' @param value The value to replace the parameters with.
#'
#' @export
formals =
function(fun)
{
  UseMethod("formals")
}

#' @export
formals.Function =
function(fun)
{
  fun$params
}

#' @export
formals.default = base::formals


#' @rdname formals
#' @export
`formals<-` =
function(fun, envir, value)
{
  UseMethod("formals<-")
}

#' @export
`formals<-.Function` =
function(fun, envir, value)
{
  fun$params = value
}

#' @export
`formals<-.default` = base::`formals<-`
