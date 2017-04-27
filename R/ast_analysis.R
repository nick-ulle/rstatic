
#' Collect All Reads in an AST
#'
#' This function collects the (unique) names of all variables that are read in
#' an AST. This function doesn't consider calling a function a read.
#'
#' Beware that there are many ASTNode subclasses that this function doesn't
#' attempt to handle; the function raises an error for unsupported classes.
#'
#' @param node (ASTNode) An abstract syntax tree.
#'
#' @export
collect_reads = function(node) {
  # FIXME: This doesn't count function names as reads.
  UseMethod("collect_reads")
}

#' @export
collect_reads.Assign = function(node) {
  collect_reads(node$read)
}

#' @export
collect_reads.Dispatch = function(node) {
  names = lapply(node$args, collect_reads)
  return (unique(unlist(names)))
}

#' @export
collect_reads.Replacement = function(node) {
  names = lapply(node$args[-1], collect_reads)
  return (unique(unlist(names)))
}

#' @export
collect_reads.Brace = function(node) {
  names = lapply(node$body, collect_reads)
  return (unique(unlist(names)))
}

#' @export
collect_reads.Symbol = function(node) {
  return (node$name)
}

#' @export
collect_reads.Literal = function(node) {
  return (character(0))
}

#' @export
collect_reads.RetTerminator = function(node) {
  collect_reads(node$value)
}

#' @export
collect_reads.BrTerminator = function(node) {
  return (character(0))
}

#' @export
collect_reads.CondBrTerminator = function(node) {
  collect_reads(node$condition)
}

#' @export
collect_reads.IterTerminator = function(node) {
  # FIXME: This shouldn't be a special case of CondBr. Loop code is generated
  # with the CFG, and that should include the loop's condition.
  collect_reads(node$iter)
}

#' @export
collect_reads.NULL = function(node) {
  return (character(0))
}

#' @export
collect_reads.default = function(node) {
  msg = sprintf(
    "Cannot collect reads for object of class '%s'.", class(node)[[1]]
  )
  stop(msg)
}
