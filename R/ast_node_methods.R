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


#' Test If Node Is Control Flow
#'
#' This function tests if a node is a control flow node.
#'
#' @param node (ASTNode) The node to test.
#'
#' @export
is_control_flow = function(node) {
  any(class(node) %in% c("If", "For", "While", "Break", "Next", "Return"))
}

is_loop = function(node) {
  any(class(node) %in% c("For", "While"))
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
