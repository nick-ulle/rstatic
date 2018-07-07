#' Apply Function to AST
#'
#' This function modifies an AST by applying a function to all nodes. The
#' result is then stored back into the same position in the AST.
#'
#' The typical use-case is to apply simple transformations to the AST without
#' having to write a method to traverse every class. The tradeoff is that this
#' function is always slower to run than a hand-written method.
#'
#' @seealso \code{\link{astTraverse}} to collect and return data about
#' nodes without modifying them.
#'
#' @param node (ASTNode) The AST.
#' @param fn (function) The function to apply to each node. The first argument
#' to the function should be the node itself.
#' @param ... Additional arguments to \code{fn}.
#' @param in_place (logical) Whether the AST should be copied before
#' transformation.
#'
node_apply = function(node, fn, ..., in_place = FALSE) {
  if (!in_place)
    node = node$copy()

  node_apply_unsafe(node, fn, ...)
}

node_apply_unsafe = function(node, fn, ...) {
  UseMethod("node_apply_unsafe")
}

node_apply_unsafe.Branch = function(node, fn, ...) {
  fn(node)
}

#' @export
node_apply_unsafe.Brace = function(node, fn, ...) {
  node$contents = lapply(node$contents, node_apply_unsafe, fn, ...)

  fn(node)
}

#' @export
node_apply_unsafe.Next = function(node, fn, ...) fn(node, ...)
#' @export
node_apply_unsafe.Break = function(node, fn, ...) fn(node, ...)

#' @export
node_apply_unsafe.If = function(node, fn, ...) {
  node$condition = node_apply_unsafe(node$condition, fn, ...)
  node$true      = node_apply_unsafe(node$true, fn, ...)
  if (!is.null(node$false))
    node$false   = node_apply_unsafe(node$false, fn, ...)

  fn(node, ...)
}

#' @export
node_apply_unsafe.For = function(node, fn, ...) {
  node$iterator = node_apply_unsafe(node$iterator, fn, ...)
  node$variable = node_apply_unsafe(node$variable, fn, ...)
  node$body = node_apply_unsafe(node$body, fn, ...)

  fn(node, ...)
}

#' @export
node_apply_unsafe.While = function(node, fn, ...) {
  node$condition = node_apply_unsafe(node$condition, fn, ...)
  node$body      = node_apply_unsafe(node$body, fn, ...)

  fn(node, ...)
}

#' @export
node_apply_unsafe.Invocation = function(node, fn, ...) {
  node$args = lapply(node$args, node_apply_unsafe, fn, ...)
  
  fn(node)
}

#' @export
node_apply_unsafe.Call = function(node, fn, ...) {
  node$fn = node_apply_unsafe(node$fn, fn, ...)

  NextMethod()
}

#' @export
node_apply_unsafe.Function = function(node, fn, ...) {
  node$body = node_apply_unsafe(node$body, fn, ...)

  fn(node, ...)
}

#' @export
node_apply_unsafe.Primitive = function(node, fn, ...) {
  node$fn = node_apply_unsafe(node$fn, fn, ...)

  NextMethod()
}

#' @export
node_apply_unsafe.Assign = function(node, fn, ...) {
  node$read  = node_apply_unsafe(node$read, fn, ...)
  node$write = node_apply_unsafe(node$write, fn, ...)

  fn(node, ...)
}

#' @export
node_apply_unsafe.Symbol = function(node, fn, ...) fn(node, ...)
#' @export
node_apply_unsafe.Parameter = function(node, fn, ...) {
  node$default = node_apply_unsafe(node$default, fn, ...)

  NextMethod()
}

#' @export
node_apply_unsafe.Literal = function(node, fn, ...) fn(node, ...)
