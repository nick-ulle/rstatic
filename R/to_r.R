#
# Methods for converting to R code.
#


#' Convert ASTNodes to R Code
#'
#' This function converts a tree of ASTNodes to the corresponding R code.
#' 
#' @param node (ASTNode) The tree to be converted.
#' @export
toR = function(node) {
  UseMethod("toR")
}


#' @export
toR.Next = function(node) {
  call("next")
}


#' @export
toR.Break = function(node) {
  call("break")
}


#' @export
toR.If = function(node) {
  if (is.null(node$false))
    call("if", toR(node$condition), toR(node$true))
  else
    call("if", toR(node$condition), toR(node$true), toR(node$false))
}


#' @export
toR.For = function(node) {
  call("for", toR(node$ivar), toR(node$iter), toR(node$body))
}


#' @export
toR.While = function(node) {
  if (node$is_repeat)
    call("repeat", toR(node$body))
  else
    call("while", toR(node$condition), toR(node$body))
}


#' @export
toR.Assign = function(node) {
  call("=", toR(node$write), toR(node$read))
}


#' @export
toR.Call = function(node) {
  fn = toR(node$fn)
  args = lapply(node$args, toR)
  as.call(append(fn, args))
}

#' @export
toR.Phi = function(node) {
  reads = lapply(node$read, toR)
  phi = as.call(append(as.name("Phi"), reads))
  call("=", toR(node$write), phi)
}

#' @export
toR.Replacement = function(node) {
  lhs = toR(node$read)

  # Delete the <- in the function name.
  fn = as.character(lhs[[1]])
  fn = gsub("<-", "", fn, fixed = TRUE)
  lhs[[1]] = as.symbol(fn)

  # Set the write variable.
  lhs[[2]] = toR(node$write)

  len = length(lhs)

  call("=", lhs[-len], lhs[[len]])
}

#' @export
toR.Return = function(node) {
  name = as.symbol("return")

  args = lapply(node$args, toR)
  as.call(append(name, args))
}

#' @export
toR.Symbol = function(node) {
  # Handle empty arguments.
  if (node$basename == "")
    return (quote(expr = ))

  as.name(node$name)
}


#' @export
toR.Parameter = function(node) {
  param =
    if (is.null(node$default))
      pairlist(quote(expr = ))
    else
      pairlist(toR(node$default))

  names(param) = node$name
  return (param)
}


#' @export
toR.Function = function(node) {
  params = pairlist()
  for (param in node$params)
    params = append(params, toR(param))

  if (is.null(node$body))
    call("function", as.pairlist(params), as.symbol(".."))
  else
    call("function", as.pairlist(params), toR(node$body))
}


#' @export
toR.Primitive = function(node) {
  .Primitive(node$fn$name)
}


#' @export
toR.Brace = function(node) {
  body = lapply(node$body, toR)
  if (node$is_paren)
    name = "("
  else
    name = "{"

  as.call(append(as.name(name), body))
}

#' @export
toR.Literal = function(node) {
  node$value
}


#' @export
toR.default = function(node) {
  # FIXME:
  msg = sprintf("Cannot convert '%s' to R code.", class(node)[1])
  stop(msg)
}
