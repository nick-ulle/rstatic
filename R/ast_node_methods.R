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


#' Check Presence of Phi Function in Block
#'
#' This function checks whether the given basic block contains a phi-function
#' for the given basename. This function is used internally to avoid duplicating
#' phi-functions during SSA construction, but may also be useful for determining
#' whether the value of the variable arrives at the block from more than one
#' execution path.
#'
#' @param block (Block) The block search for phi-functions.
#' @param x (character) The basename to match against phi-functions in the
#' block.
#'
#' @export
has_phi = function(block, x) {
  # Check if there's a phi-function in block for name x.
  match = vapply(block$phi, function(node) {
    node$write$basename == x
  }, NA)

  any(match)
}
