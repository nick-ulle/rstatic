
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
collectReads = function(node) {
  UseMethod("collectReads")
}

#' @export
collectReads.Assign = function(node) {
  collectReads(node$read)
}

#' @export
collectReads.Phi = function(node) {
  names = unlist(lapply(node$read, collectReads))

  unique(names)
}

#' @export
collectReads.Application = function(node) {
  names = unlist(lapply(node$args, collectReads))

  unique(names)
}

#' @export
collectReads.Call = function(node) {
  union(collectReads(node$fn), NextMethod())
}

#' @export
collectReads.Brace = function(node) {
  names = unlist(lapply(node$body, collectReads))

  unique(names)
}

#' @export
collectReads.Symbol = function(node) {
  node$name
}

#' @export
collectReads.Parameter = function(node) {
  collectReads(node$default)
}

#' @export
collectReads.Literal = function(node) {
  character(0)
}

#' @export
collectReads.RetTerminator = function(node) {
  collectReads(node$value)
}

#' @export
collectReads.BrTerminator = function(node) {
  character(0)
}

#' @export
collectReads.CondBrTerminator = function(node) {
  collectReads(node$condition)
}

#' @export
collectReads.IterTerminator = function(node) {
  # FIXME: This shouldn't be a special case of CondBr. Loop code is generated
  # with the CFG, and that should include the loop's condition.
  collectReads(node$iter)
}

#' @export
collectReads.NULL = function(node) {
  character(0)
}

#' @export
collectReads.Function = function(node) {
  character(0)
}

#' @export
collectReads.default = function(node) {
  msg = sprintf(
    "Cannot collect reads for object of class '%s'.", class(node)[[1]]
  )
  stop(msg)
}
