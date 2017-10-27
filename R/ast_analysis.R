# FIXME: Add parameter to control whether sub-blocks are checked.
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
  UseMethod("collect_reads")
}

#' @export
collect_reads.Assign = function(node) {
  collect_reads(node$read)
}

#' @export
collect_reads.Phi = function(node) {
  names = unlist(lapply(node$read, collect_reads))

  unique(names)
}

#' @export
collect_reads.Application = function(node) {
  names = unlist(lapply(node$args, collect_reads))

  unique(names)
}

#' @export
collect_reads.Call = function(node) {
  union(collect_reads(node$fn), NextMethod())
}

#' @export
collect_reads.Brace = function(node) {
  names = unlist(lapply(node$body, collect_reads))

  unique(names)
}

#' @export
collect_reads.Symbol = function(node) {
  node$name
}

#' @export
collect_reads.Parameter = function(node) {
  collect_reads(node$default)
}

#' @export
collect_reads.Literal = function(node) {
  character(0)
}

#' @export
collect_reads.If = function(node) {
  # Only collect for condition, not sub-blocks.
  collect_reads(node$condition)
}

#' @export
collect_reads.Loop = function(node) {
  # Since an If is generated in the loop's test block, no need to collect reads
  # here.
  character(0)
}

#' @export
collect_reads.NULL = function(node) {
  character(0)
}

#' @export
collect_reads.Function = function(node) {
  character(0)
}

#' @export
collect_reads.default = function(node) {
  msg = sprintf(
    "Cannot collect reads for object of class '%s'.", class(node)[[1]]
  )
  stop(msg)
}
