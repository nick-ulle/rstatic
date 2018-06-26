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
function(node, check, ..., recursive = TRUE, .address = integer(0))
{
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


#' Replace Nodes in an ASTNode Object
#'
#' This function replaces each node of an ASTNode object (including the object
#' itself) with the result of calling the given function on the node.
#'
#' Nodes are replaced from the bottom up, so the given node is replaced last.
#'
#' @param node (ASTNode) The code object to modify.
#' @param fn (function) The function to call on each node.
#' @examples
#' rename_symbols = function(node, name, newname)
#' { # Rename symbols to something else.
#'   if (is(node, "Symbol") && node$basename == name)
#'     node$basename = newname
#'
#'   node
#' }
#'
#' ast = quote_ast(ans <- sum(x, y / x, z))
#'
#' replace_nodes(ast, rename_symbols, "x", "newx")
#' @export
replace_nodes =
function(node, fn, ...)
{
  for (i in seq_along(node)) {
    node[[i]] = replace_nodes(node[[i]], fn, ...)
  }

  fn(node, ...)
}


insert_nodes =
function(container, new_nodes, check = identical, ..., after = TRUE)
{
  if (!is(container, "Container")) {
    msg = sprintf("Cannot insert into class '%s'.", toString(class(container)))
    stop(msg)
  }

  # Find identical
  w = vapply(container$contents, check, NA, ...)

  if (!any(w))
    stop("No matches found in the container.")

  w = match(TRUE, w)
  if (!after)
    w = w - 1

  container$contents = append(lines, new_node, after = w)

  NULL
}
