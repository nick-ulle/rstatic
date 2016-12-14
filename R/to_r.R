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
  # FIXME: func should probably be an AST object regardless of whether it's a
  # symbol or not.
  func = node$func
  if (is.character(func))
    func = as.name(func)
  else
    func = to_r(func)

  args = lapply(node$args, to_r)
  as.call(append(func, args))
}

#' @export
to_r.Phi = function(node) {
  args = lapply(node$args, to_r)
  as.call(append(as.name("Phi"), args))
}

#' @export
to_r.Replacement = function(node) {
  args = lapply(node$args, to_r)
  len = length(args)

  # FIXME: Check that node$func is a string.
  func = gsub("<-", "", node$func, fixed = TRUE)
  write = do.call(call, append(func, args[-len]), quote = TRUE)
  call("=", write, args[[len]])
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
  # Handle empty arguments.
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
to_r.Brace = function(node) {
  body = lapply(node$body, to_r)
  if (node$is_paren)
    name = "("
  else
    name = "{"

  do.call(call, append(name, body), quote = TRUE)
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
