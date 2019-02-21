#' @include blocks_to_r.R
NULL

#' @rdname as_language
#' @usage NULL
#' @export
to_r =
function(node, ...)
{
  .Deprecated("as_language")
  UseMethod("as_language")
}

#' Convert to R Language Objects
#'
#' This function converts rstatic intermediate representations back to R
#' language objects.
#' 
#' @param node The intermediate representation to convert.
#' @param ... Optional arguments to and from methods.
#'
#' @export
as_language =
function(node, ...) {
  UseMethod("as_language")
}

#' @export
as_language.BlockList = blocks_to_r.BlockList

#' @export
as_language.data.frame = blocks_to_r.data.frame


as_language.Label =
function(node, ...) {
  # NOTE: Labels should not appear in real R code.
  as.name(node$name)
}

#' @export
as_language.Brace =
function(node, ..., keep_braces = FALSE) {
  contents = lapply(node$contents, as_language, ...)

  if (!keep_braces && node$is_hidden && length(contents) == 1L)
    contents[[1L]]
  else
    as.call(c(as.name("{"), contents))
}


#' @export
as_language.Parenthesis =
function(node, ...) {
  call("(", as_language(node$args[[1]], ...))
}

as_language.Branch =
function(node, ...) {
  # NOTE: An empty list means no code.
  list()
}

#' @export
as_language.Next =
function(node, ...) {
  call("next")
}

#' @export
as_language.Break =
function(node, ...) {
  call("break")
}

#' @export
as_language.Return =
function(node, ...) {
  call("return", as_language(node$read, ...))
}


#' @export
as_language.If =
function(node, ...) {
  true = as_language(node$true, ...)
  false = as_language(node$false, ...)
  condition = as_language(node$condition, ...)

  call("if", condition, true, false)
}

#' @export
as_language.For = function(node, ...) {
  body = as_language(node$body, ...)
  variable = as_language.Symbol(node$variable, ...)
  iterator = as_language(node$iterator, ...)

  call("for", variable, iterator, body)
}


#' @export
as_language.While =
function(node, ...) {
  body = as_language.Brace(node$body)

  if (node$is_repeat) {
    call("repeat", body)
  } else {
    condition = as_language(node$condition, ...)
    call("while", condition, body)
  }
}


#' @export
as_language.Assign =
function(node, ...) {
  read = as_language(node$read, ...)
  write = as_language(node$write, ...)

  call("=", write, read)
}


#' @export
as_language.Call =
function(node, ...) {
  fn = as_language(node$fn, ...)
  args = as_language(node$args, ...)

  as.call(append(fn, args))
}

#' @export
as_language.ArgumentList =
function(node, ...) {
  lapply(node$contents, as_language, ...)
}

#' @export
as_language.Phi =
function(node, ...) {
  reads = lapply(node$read, as_language, ...)
  phi = as.call(append(as.name("Phi"), reads))
  call("=", as_language(node$write, ...), phi)
}

#' @export
as_language.Replacement =
function(node, ...) {
  lhs = as_language(node$read, ...)

  # Delete the <- in the function name.
  fn = as.character(lhs[[1]])
  fn = gsub("<-", "", fn, fixed = TRUE)
  lhs[[1]] = as.symbol(fn)

  # Set the write variable.
  lhs[[2]] = as_language(node$write, ...)

  len = length(lhs)

  call("=", lhs[-len], lhs[[len]])
}

#' @export
as_language.Symbol =
function(node, ..., keep_ssa = FALSE) {
  # Handle empty arguments.
  if (node$value == "")
    return (quote(expr = ))

  if (keep_ssa)
    name = node$ssa_name
  else
    name = node$value

  if (is.na(node$namespace))
    as.name(name)
  else
    call(node$namespace_fn$ssa_name, as.name(node$namespace), as.name(name))
}


#' @export
as_language.Parameter =
function(node, ...) {
  default = pairlist(as_language(node$default, ...))
  names(default) = node$ssa_name

  default
}

as_language.ParameterList =
function(node, ...)
{
  # NOTE: Can't use lapply() here because it breaks the pairlist.
  params = NULL
  for (param in node$contents)
    params = c(params, as_language(param, ...))

  as.pairlist(params)
}

#' @export
as_language.Function =
function(node, ...)
{
  call("function", as_language(node$params, ...), as_language(node$body, ...))
}


#' @export
as_language.Primitive =
function(node, ...) {
  .Primitive(node$fn$ssa_name)
}


#' @export
as_language.Literal =
function(node, ...) {
  node$value
}
