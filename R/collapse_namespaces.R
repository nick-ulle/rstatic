
#' Collapse Namespaces
#'
#' This function collapses namespaces in the AST, so that they are stored in
#' the namespace field of each Symbol rather than as a call to \code{::} or
#' \code{:::}.
#'
#' @param node (ASTNode) The AST.
#' @param in_place (logical) Whether the AST should be copied before
#' transformation.
#'
#' @export
collapse_namespaces = function(node, in_place = FALSE) {
  if (in_place && is(node, "Namespace"))
    warning("in_place = TRUE and root node is a Namespace.\n",
      "  Use node$args[[1]] to get the new root node.")

  node_apply(node, collapse_node_namespaces, in_place = in_place)
}

#' Collapse A Namespace Node
#'
#' This function will collapse a Namespace node onto the Symbol in its second
#' argument. Typically this will be used with \code{node_apply()} or through
#' the frontend \code{collapse_namespaces()}.
#'
#' @param node (ASTNode) The ASTNode to collapse.
#' @export
collapse_node_namespaces = function(node, ...) {
  UseMethod("collapse_node_namespaces")
}

#' export
collapse_node_namespaces.Namespace = function(node, ...) {
  name = node$args[[2]]

  name$namespace = node$args[[1]]$name
  name$parent = node$parent

  node$fn$parent = name
  name$namespace_fn = node$fn

  name
}

#' export
collapse_node_namespaces.default = function(node, ...) node
