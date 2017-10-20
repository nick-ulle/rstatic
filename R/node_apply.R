#' Apply Function to AST
#'
#' This function applies a function to all elements of an AST. The result is
#' then stored back into the same position in the AST.
#'
#' The typical use-case is to apply simple transformations to the AST without
#' having to write a method to traverse every class. The tradeoff is that this
#' function is always slower to run than a hand-written method.
#'
#' If you want to collect and return data about nodes without modifying them,
#' then see \code{nodeCollect()}.
#'
#' @param node (ASTNode) The AST.
#' @param fn (function) The function to apply to each node. The first argument
#' to the function should be the node itself.
#' @param ... Additional arguments to \code{fn}.
#' @param inPlace (logical) Whether the AST should be copied before
#' transformation.
#'
#' @export
nodeApply = function(node, fn, ..., inPlace = FALSE) {
  if (!inPlace)
    node = node$copy()

  nodeApplyUnsafe(node, fn, ...)
}

nodeApplyUnsafe = function(node, fn, ...) {
  UseMethod("nodeApplyUnsafe")
}

#' @export
nodeApplyUnsafe.Brace = function(node, fn, ...) {
  node$body = lapply(node$body, nodeApplyUnsafe, fn, ...)

  fn(node)
}

#' @export
nodeApplyUnsafe.Next = function(node, fn, ...) fn(node, ...)
#' @export
nodeApplyUnsafe.Break = function(node, fn, ...) fn(node, ...)

#' @export
nodeApplyUnsafe.If = function(node, fn, ...) {
  node$condition = nodeApplyUnsafe(node$condition, fn, ...)
  node$true      = nodeApplyUnsafe(node$true, fn, ...)
  node$false     = nodeApplyUnsafe(node$false, fn, ...)

  fn(node, ...)
}

#' @export
nodeApplyUnsafe.For = function(node, fn, ...) {
  node$iter = nodeApplyUnsafe(node$iter, fn, ...)
  node$ivar = nodeApplyUnsafe(node$ivar, fn, ...)
  node$body = nodeApplyUnsafe(node$body, fn, ...)

  fn(node, ...)
}

#' @export
nodeApplyUnsafe.While = function(node, fn, ...) {
  node$condition = nodeApplyUnsafe(node$condition, fn, ...)
  node$body      = nodeApplyUnsafe(node$body, fn, ...)

  fn(node, ...)
}

#' @export
nodeApplyUnsafe.Application = function(node, fn, ...) {
  node$args = lapply(node$args, nodeApplyUnsafe, fn, ...)
  
  fn(node)
}

#' @export
nodeApplyUnsafe.Call = function(node, fn, ...) {
  node$fn = nodeApplyUnsafe(node$fn, fn, ...)

  NextMethod()
}

#' @export
nodeApplyUnsafe.Callable = function(node, fn, ...) {
  node$body = nodeApplyUnsafe(node$body, fn, ...)

  fn(node, ...)
}

#' @export
nodeApplyUnsafe.Primitive = function(node, fn, ...) {
  node$fn = nodeApplyUnsafe(node$fn, fn, ...)

  NextMethod()
}

#' @export
nodeApplyUnsafe.Assign = function(node, fn, ...) {
  node$read  = nodeApplyUnsafe(node$read, fn, ...)
  node$write = nodeApplyUnsafe(node$write, fn, ...)

  fn(node, ...)
}

#' @export
nodeApplyUnsafe.Symbol = function(node, fn, ...) fn(node, ...)
#' @export
nodeApplyUnsafe.Parameter = function(node, fn, ...) {
  node$default = nodeApplyUnsafe(node$default, fn, ...)

  NextMethod()
}

#' @export
nodeApplyUnsafe.Literal = function(node, fn, ...) fn(node, ...)
