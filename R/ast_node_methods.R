#' @export
`[.Container` = function(x, i, ...) {
  x$body[i, ...]
}

#' @export
`[[.Container` = function(x, i, ...) {
  x$body[[i, ...]]
}

#' @export
`[[<-.Container` = function(x, i, ..., value) {
  value = .reparent_ast(value, x)
  x$body[[i, ...]] = value
  x
}

#' @export
length.Container = function(x) {
  length(x$body)
}


#' @export
as_blocks = function(node) {
  UseMethod("as_blocks")
}

#' @export
as_blocks.ASTNode = function(node) {
  Brace$new(node, parent = node$parent)
}

#' @export
as_blocks.Brace = function(node) node

#' @export
as_blocks.BlockList = function(node) node

#' @export
as_blocks.NULL = function(node) node
