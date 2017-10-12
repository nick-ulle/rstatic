nodeApply = function(node, fn) {
  UseMethod("nodeApply")
}

#' @export
nodeApply.Brace = function(node, fn) {
  lapply(node$body, nodeApply, fn)

  fn(node)
}

#' @export
nodeApply.Next = function(node, fn) fn(node)
#' @export
nodeApply.Break = function(node, fn) fn(node)

#' @export
nodeApply.If = function(node, fn) {
  nodeApply(node$condition, fn)
  nodeApply(node$true, fn)
  nodeApply(node$false, fn)

  fn(node)
}

#' @export
nodeApply.For = function(node, fn) {
  nodeApply(node$iter, fn)
  nodeApply(node$ivar, fn)
  nodeApply(node$body, fn)

  fn(node)
}

#' @export
nodeApply.While = function(node, fn) {
  nodeApply(node$condition, fn)
  nodeApply(node$body, fn)

  fn(node)
}

#' @export
nodeApply.Application = function(node, fn) {
  lapply(node$args, nodeApply, fn)
  
  fn(node)
}

#' @export
nodeApply.Call = function(node, fn) {
  nodeApply(node$fn, fn)

  NextMethod()
}

#' @export
nodeApply.Callable = function(node, fn) {
  nodeApply(node$body, fn)

  fn(node)
}

#' @export
nodeApply.Primitive = function(node, fn) {
  nodeApply(node$fn, fn)

  NextMethod()
}

#' @export
nodeApply.Assign = function(node, fn) {
  nodeApply(node$read, fn)
  nodeApply(node$write, fn)

  fn(node)
}

#' @export
nodeApply.Symbol = function(node, fn) fn(node)
#' @export
nodeApply.Parameter = function(node, fn) {
  nodeApply(node$default, fn)

  NextMethod()
}

#' @export
nodeApply.Literal = function(node, fn) fn(node)
