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
    call("if", to_r(node$predicate), to_r(node$true))
  else
    call("if", to_r(node$predicate), to_r(node$true), to_r(node$false))
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
    call("while", to_r(node$predicate), to_r(node$body))
}


#' @export
to_r.Assign = function(node) {
  call("=", to_r(node$write), to_r(node$read))
}


#' @export
to_r.Call = function(node) {
  args = lapply(node$args, to_r)
  do.call(call, append(node$name, args), quote = TRUE)
}

#' @export
to_r.Replacement = function(node) {
  args = lapply(node$args, to_r)
  len = length(args)

  name = gsub("<-", "", node$name, fixed = TRUE)
  write = do.call(call, append(name, args[-len]), quote = TRUE)
  call("=", write, args[[len]])
}

#' @export
to_r.Return = to_r.Call

#' @export
to_r.Internal = function(node) {
  # NOTE: This is a workaround because it's illegal to construct calls to
  # .Internal.
  node$name = "."
  code = NextMethod()
  code[[1]] = quote(.Internal)
  return (code)
}


#' @export
to_r.Symbol = function(node) {
  if (node$name == "")
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
  .Primitive(node$name)
}


#' @export
to_r.Bracket = function(node) {
  body = lapply(node$body, to_r)
  do.call(call, append("{", body), quote = TRUE)
}


#' @export
to_r.Paren = function(node) {
  call("(", to_r(node$body))
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
