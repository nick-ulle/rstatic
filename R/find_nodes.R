#' Find Nodes in an RStatic Object
#'
#' This function traverses the given RStatic object and returns a list of
#' indices for all descendants that satisfy the given test function.
#'
#' All indices are relative to \code{node}, unless \code{index} is supplied.
#'
#' Note that \code{node} itself is not tested.
#'
#' @param node (ASTNode) The RStatic object to search.
#' @param test (function) The test function, which should accept an rstatic
#' object to check as its first argument and return a scalar logical.
#' @param ... Additional arguments to the test function.
#' @param recursive (logical) Search recursively in matching nodes?
#' @param initial (list) Initial list of match indices.
#' @param index (integer) The index of \code{node}.
#'
#' @return A list of indices for matching nodes.
#' @examples
#' ast = quote_ast(
#'   function(x, y) {
#'     x = x + y
#'     function() x
#'   })
#'
#' find_nodes(ast, is, "Function")
#'
#' find_nodes(ast, function(node) is(node, "Symbol") && node$value == "x")
#' @export
find_nodes =
function(
  node
  , test
  , ..., recursive = TRUE, initial = list()
  , index = integer(0)
  )
{
  if (length(index) && test(node, ...)) {
    initial = c(initial, list(index))

    if (!recursive)
      return (initial)
  }

  children = children(node)

  for (i in seq_along(children)) {
    initial = find_nodes(children[[i]], test, ..., recursive = recursive
      , initial = initial, index = c(index, i))
  }

  initial
}


#' Replace Nodes in an RStatic Object
#'
#' This function traverses the given RStatic object, calls the given replace
#' function on each node, and replaces each node with the result of the call.
#'
#' Replacement happens from the bottom up, so \code{node} is replaced last.
#'
#' @param node (ASTNode) The RStatic object to traverse.
#' @param replace (function) The function to call on each node.
#' @param ... Additional arguments to the replace function.
#' @param in_place (logical) Copy \code{node} before replacement?
#'
#' @examples
#' rename_symbols = function(node, name, newname)
#' { # Rename symbols to something else.
#'   if (is(node, "Symbol") && node$value == name)
#'     node$value = newname
#'
#'   node
#' }
#'
#' ast = quote_ast(ans <- sum(x, y / x, z))
#'
#' replace_nodes(ast, rename_symbols, "x", "newx")
#' @export
replace_nodes =
function(node, replace, ..., in_place = FALSE)
{
  if (!in_place)
    node = copy(node)

  children(node) = lapply(children(node), replace_nodes, replace, ...
    , in_place = TRUE)
  replace(node, ...)
}


#' Get Index of RStatic Object
#'
#' This function gets the index of the given RStatic object relative to its
#' ancestor nodes.
#'
#' @param node (ASTNode) The RStatic object to traverse.
#' @param index (integer) Initial index. The value of this parameter is
#' appended to the end of the computed index.
#'
#' @return An integer vector which can be used as an index in the \code{child}
#' function.
#'
#' @examples
#' ast = quote_ast(x <- y + z)
#' node = child(ast, c(2, 2, 1))
#'
#' get_index(node)
#' @export
get_index =
function(node, index = integer(0))
{
  parent = node$parent
  if (is.null(parent))
    return (index)

  children = children(parent)
  for (i in seq_along(children)) {
    child = children[[i]]
    if (identical(node, child)) {
      index = get_index(parent, index = c(i, index))
      return (index)
    }
  }

  NA_integer_
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
