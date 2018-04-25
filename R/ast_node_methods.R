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
  value = set_parent(value, x)
  x$body[[i, ...]] = value
  x
}

#' @export
length.Container = function(x) {
  length(x$body)
}


#' Wrap ASTNode With Brace
#'
#' This function wraps an ASTNode in a Brace if it isn't one already.
#'
#' @export
wrap_brace = function(node) {
  if (is(node, "Brace"))
    node
  else
    Brace$new(node, parent = node$parent)
}
