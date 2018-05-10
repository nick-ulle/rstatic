#' @include blocks_to_r.R
NULL

#' Convert Intermediate Representations to R Code
#'
#' This function converts rstatic intermediate representations back to R code.
#' 
#' @param node The intermediate representation to convert.
#' @param ... Optional arguments to and from methods.
#'
#' @export
to_r =
function(node, ...) {
  UseMethod("to_r")
}

#' @export
to_r.BlockList = blocks_to_r.BlockList

#' @export
to_r.data.frame = blocks_to_r.data.frame


to_r.Label =
function(node, ...) {
  # NOTE: Labels should not appear in real R code.
  as.name(node$name)
}

#' @export
to_r.Brace =
function(node, ..., keep_braces = FALSE) {
  body = lapply(node$body, to_r, ...)

  if (!keep_braces && node$is_hidden && length(body) == 1L)
    body[[1L]]
  else
    as.call(c(as.name("{"), body))
}


#' @export
to_r.Parenthesis =
function(node, ...) {
  call("(", to_r(node$args[[1]], ...))
}

to_r.Branch =
function(node, ...) {
  # NOTE: An empty list means no code.
  list()
}

#' @export
to_r.Next =
function(node, ...) {
  call("next")
}

#' @export
to_r.Break =
function(node, ...) {
  call("break")
}

#' @export
to_r.Return =
function(node, ...) {
  call("return", to_r(node$read, ...))
}


#' @export
to_r.If =
function(node, ...) {
  true = to_r(node$true, ...)
  false = to_r(node$false, ...)
  condition = to_r(node$condition, ...)

  call("if", condition, true, false)
}

#' @export
to_r.For = function(node, ...) {
  body = to_r(node$body, ...)
  variable = to_r.Symbol(node$variable, ...)
  iterator = to_r(node$iterator, ...)

  call("for", variable, iterator, body)
}


#' @export
to_r.While =
function(node, ...) {
  body = to_r.Brace(node$body)

  if (node$is_repeat) {
    call("repeat", body)
  } else {
    condition = to_r(node$condition, ...)
    call("while", condition, body)
  }
}


#' @export
to_r.Assign =
function(node, ...) {
  read = to_r(node$read, ...)
  write = to_r(node$write, ...)

  call("=", write, read)
}


#' @export
to_r.Call =
function(node, ...) {
  fn = to_r(node$fn, ...)
  args = lapply(node$args, to_r, ...)

  as.call(append(fn, args))
}

#' @export
to_r.Phi =
function(node, ...) {
  reads = lapply(node$read, to_r, ...)
  phi = as.call(append(as.name("Phi"), reads))
  call("=", to_r(node$write, ...), phi)
}

#' @export
to_r.Replacement =
function(node, ...) {
  lhs = to_r(node$read, ...)

  # Delete the <- in the function name.
  fn = as.character(lhs[[1]])
  fn = gsub("<-", "", fn, fixed = TRUE)
  lhs[[1]] = as.symbol(fn)

  # Set the write variable.
  lhs[[2]] = to_r(node$write, ...)

  len = length(lhs)

  call("=", lhs[-len], lhs[[len]])
}

#' @export
to_r.Symbol =
function(node, ..., keep_ssa = FALSE) {
  # Handle empty arguments.
  if (node$basename == "")
    return (quote(expr = ))

  if (keep_ssa)
    name = node$name
  else
    name = node$basename

  if (is.na(node$namespace))
    as.name(name)
  else
    call(node$namespace_fn$name, as.name(node$namespace), as.name(name))
}


#' @export
to_r.Parameter =
function(node, ...) {
  if (is.null(node$default))
    default = pairlist(quote(expr = ))
  else
    default = pairlist(to_r(node$default, ...))

  names(default) = node$name
  default
}

to_r_params =
function(params_list, ...) {
  params = pairlist()
  for (param in params_list)
    params = append(params, to_r(param, ...))

  as.pairlist(params)
}

#' @export
to_r.Function =
function(node, ...) {
  call("function", to_r_params(node$params, ...), to_r(node$body, ...))
}


#' @export
to_r.Primitive =
function(node, ...) {
  .Primitive(node$fn$name)
}


#' @export
to_r.Literal =
function(node, ...) {
  node$value
}
