#' Collapse Namespace
#'
#' If \code{node} is a Namespace (a call to \code{::} or \code{:::}), this
#' function returns the Symbol the Namespace affects and stores associated
#' namespace information in the \code{namespace} field of that Symbol. If
#' \code{node} is not a Namespace, this function returns \code{node} unchanged.
#'
#' This function is intended as a replace function for \code{replace_nodes()}.
#'
#' @param node (ASTNode) The node to collapse.
#' @param ... Unused arguments.
#'
#' @examples
#' code = quote_ast(rstatic::collapse_one_namespace)
#' replace_nodes(code, collapse_namespace)
#' @export
collapse_namespace =
function(node, ...)
{
  UseMethod("collapse_namespace")
}

#' export
collapse_namespace.Namespace =
function(node, ...)
{
  name = node$args[[2]]

  name$namespace = node$args[[1]]$name
  name$parent = node$parent

  node$fn$parent = name
  name$namespace_fn = node$fn

  name
}

#' export
collapse_namespace.default =
function(node, ...)
{
  node
}
