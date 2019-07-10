#' Match Call Arguments to Parameters
#'
#' This function matches the arguments in an unevaluated call to the parameters
#' in a function definition.
#'
#' This function is a wrapper for [match.call()] that adds support for:
#'
#' * Primitive functions
#' * Anonymous functions
#' * Automatic definition lookup based on the call
#' * `ASTNode` objects as the call or the defintion
#'
#' @param node An unevaluated call with arguments to match.
#' @param definition Optional definition for the called function.
#' @param expand_dots (logical) Should arguments matching `...` in the call be
#' expanded or left as a `...` argument?
#' @param envir (environment) An environment where [find_function()] can look
#' up function definition. Only used if `definition` is missing.
#'
#' @return An object of the same class as `node`, with full parameter names for
#' all arguments.
#'
#' @seealso [match.call()], [find_function()]
#'
#' @export
match_call =
function(node, definition, expand_dots, envir)
{
  UseMethod("match_call")
}


#' @rdname match_call
#' @export
match_call.call =
function(
  node
  , definition = find_function(node, envir)
  , expand_dots = TRUE
  , envir = parent.frame())
{
  # Convert definition to a function match.call can use.
  if (is.primitive(definition)) {
    # Convert primitive to a stub function for match.call().
    stub = args(definition)
    if (is.null(stub)) {
      if (identical(definition, base::`[`))
        stub = function(x, i, j, ..., drop = TRUE) NULL
      else if (identical(definition, base::`[[`))
        stub = function(x, i, j, ..., exact = TRUE) NULL
      else if (identical(definition, base::`$`))
        stub = function(x, name) NULL
      else
        stop("This primitive is not yet supported by match_call().
          Please contact the package maintainer for help.")
    }

    definition = stub

  } else if (inherits(definition, "Function")) {
    fn = as_language(definition)
    definition = eval(fn, envir = new.env(emptyenv()))

  } else if (!is.function(definition)) {
    cl = class(definition)[[1L]]
    stop(sprintf("Invalid class '%s' for definition", cl))
  }

  match.call(definition, node)
}


#' @rdname match_call
#' @export
match_call.Call =
function(
  node
  , definition = find_function(node, envir)
  , expand_dots = TRUE
  , envir = parent.frame())
{
  matched = match_call.call(as_language(node), definition
    , expand_dots = expand_dots, envir = envir)
  to_ast(matched)
}



#' Find Function Definition for Call
#'
#' This function returns the definition of the function to be called by an
#' unevaluated call.
#'
#' This function raises an error if it is unable to find the definition.
#'
#' @param node (call|Call) The unevaluated call.
#' @param envir (environment) An environment to look up the definition.
#' @param top (logical) Is this the top-level call to `find_function()`? This
#' parameter should not be set by the user.
#'
#' @return A function.
#'
#' @export
find_function =
function(node, envir, top)
{
  UseMethod("find_function")
}


#' @rdname find_function
#' @export
find_function.call =
function(node, envir = parent.frame(), top = TRUE)
{
  find_function.Call(to_ast(node), envir, top)
}


#' @rdname find_function
#' @export
find_function.Call =
function(node, envir = parent.frame(), top = TRUE)
{
  if (top)
    find_function(node$fn, envir, top = FALSE)
  else
    stop("Unable to find function definition.")
}


#' @export
find_function.Brace =
function(node, envir = parent.frame(), top = FALSE)
{
  find_function(node$contents[[length(node$contents)]], envir, top)
}


#' @export
find_function.Parenthesis =
function(node, envir = parent.frame(), top = FALSE)
{
  find_function(node$args[[length(node$args)]], envir, top)
}


#' @export
find_function.Symbol =
function(node, envir = parent.frame(), top = FALSE)
{
  get(node$value, envir = envir)
}


#' @export
find_function.Function =
function(node, envir = parent.frame(), top = FALSE)
{
  fn = as_language(node)
  eval(fn, envir = new.env(emptyenv()))
}


#' @export
find_function.ASTNode =
function(node, envir = parent.frame(), top = FALSE)
{
  stop("Unable to find function definition.")
}
