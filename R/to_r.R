#
# Methods for converting to R code.
#


#' Convert ASTNodes to R Code
#'
#' This function converts a tree of ASTNodes to the corresponding R code.
#' 
#' @param node (ASTNode) The tree to be converted.
#' @export
to_r = function(node) {
  UseMethod("to_r")
}


#' @export
to_r.Next = function(node) {
  call("next")
}


#' @export
to_r.Break = function(node) {
  call("break")
}


#' @export
to_r.If = function(node) {
  if (is.null(node$false))
    call("if", to_r(node$condition), to_r(node$true))
  else
    call("if", to_r(node$condition), to_r(node$true), to_r(node$false))
}


#' @export
to_r.For = function(node) {
  call("for", to_r(node$ivar), to_r(node$iter), to_r(node$body))
}


#' @export
to_r.While = function(node) {
  if (node$is_repeat)
    call("repeat", to_r(node$body))
  else
    call("while", to_r(node$condition), to_r(node$body))
}


#' @export
to_r.Assign = function(node) {
  call("=", to_r(node$write), to_r(node$read))
}


#' @export
to_r.Call = function(node) {
  fn = to_r(node$fn)
  args = lapply(node$args, to_r)
  as.call(append(fn, args))
}

#' @export
to_r.Phi = function(node) {
  reads = lapply(node$read, to_r)
  phi = as.call(append(as.name("Phi"), reads))
  call("=", to_r(node$write), phi)
}

#' @export
to_r.Replacement = function(node) {
  lhs = to_r(node$read)

  # Delete the <- in the function name.
  fn = as.character(lhs[[1]])
  fn = gsub("<-", "", fn, fixed = TRUE)
  lhs[[1]] = as.symbol(fn)

  # Set the write variable.
  lhs[[2]] = to_r(node$write)

  len = length(lhs)

  call("=", lhs[-len], lhs[[len]])
}

#' @export
to_r.Return = function(node) {
  if (node$is_invisible)
    name = as.symbol("invisible")
  else
    name = as.symbol("return")

  args = lapply(node$args, to_r)
  as.call(append(name, args))
}

#' @export
to_r.Symbol = function(node) {
  # Handle empty arguments.
  if (node$basename == "")
    return (quote(expr = ))

  as.name(node$name)
}


#' @export
to_r.Parameter = function(node) {
  param =
    if (is.null(node$default))
      pairlist(quote(expr = ))
    else
      pairlist(to_r(node$default))

  names(param) = node$name
  return (param)
}


#' @export
to_r.Function = function(node) {
  # TODO: Is there a better way to do this?
  params = list()
  for (param in node$params)
    params = append(params, to_r(param))

  call("function", as.pairlist(params), to_r(node$body))
}


#' @export
to_r.Primitive = function(node) {
  .Primitive(node$fn$name)
}


#' @export
to_r.Brace = function(node) {
  body = lapply(node$body, to_r)
  if (node$is_paren)
    name = "("
  else
    name = "{"

  as.call(append(as.name(name), body))
}

#' @export
to_r.Literal = function(node) {
  node$value
}


#' @export
to_r.default = function(node) {
  # FIXME:
  msg = sprintf("Cannot convert '%s' to R code.", class(node)[1])
  stop(msg)
}
