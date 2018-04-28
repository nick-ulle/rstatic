#' Find Nodes in an ASTNode Object
#'
#' This function returns a list of addresses for all children of an ASTNode
#' object that satisfy the given check function.
#'
#' Note that all adresses are relative to \code{node}, and \code{node} itself
#' is not checked.
#'
#' @param node (ASTNode) The code object to search.
#' @param check (function) The check function, which should take the ASTNode
#' object to check as its first argument and return a scalar logical.
#' @param ... Additional arguments to the check function.
#' @param recursive (logical) Search recursively in matching nodes?
#'
#' @return A list of addresses for matching nodes.
#' @examples
#' ast = quote_ast(
#'   function(x, y) {
#'     x = x + y
#'     function() x
#'   })
#'
#' find_nodes(ast, is, "Function")
#'
#' find_nodes(ast, function(node) is(node, "Symbol") && node$name == "x")
#' @export
find_nodes =
function(node, check, ..., recursive = TRUE, .address = integer(0)) {
  # NOTE: It is a tiny bit faster to have this function take an initial set of
  # matches as an argument and return a modified version.

  matches = list()

  # Add node address to results
  if (length(.address) && check(node, ...)) {
    matches = c(matches, list(.address))

    if (!recursive)
      return (matches)
  }

  for (i in seq_along(node)) {
    new_matches = find_nodes(node[[i]], check, ..., recursive = recursive,
      .address = c(.address, i))
    matches = c(matches, new_matches)
  }

  matches
}
