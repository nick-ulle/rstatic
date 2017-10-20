
#' Collapse Namespaces
#'
#' This function collapses namespaces in the AST, so that they are stored in
#' the namespace field of each Symbol rather than as a call to \code{::} or
#' \code{:::}.
#'
#' @param node (ASTNode) The AST.
#' @param inPlace (logical) Whether the AST should be copied before
#' transformation.
#'
#' @export
collapseNamespaces = function(node, inPlace = FALSE) {
  if (inPlace && inherits(node, "Namespace"))
    warning("inPlace = TRUE and root node is a Namespace.\n",
      "  Use node$args[[1]] to get the new root node.")

  nodeApply(node, collapseNodeNamespaces, inPlace = inPlace)
}

#' Collapse A Namespace Node
#'
#' This function will collapse a Namespace node onto the Symbol in its second
#' argument. Typically this will be used with \code{nodeApply()} or through the
#' frontend \code{collapseNamespaces()}.
#'
#' @param node (ASTNode) The ASTNode to collapse.
#' @export
collapseNodeNamespaces = function(node, ...) {
  UseMethod("collapseNodeNamespaces")
}

#' export
collapseNodeNamespaces.Namespace = function(node, ...) {
  name = node$args[[2]]

  name$namespace = node$args[[1]]$name
  name$parent = node$parent

  node$fn$parent = name
  name$namespace_fn = node$fn

  name
}

#' export
collapseNodeNamespaces.default = function(node, ...) node
