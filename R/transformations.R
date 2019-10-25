#' Replace a Node in its Parent Node
#'
#' This function replaces an ASTNode object in its parent expression with
#' another ASTNode object.
#'
#' By default, this is an in-place transformation that modifies `node` as a
#' side effect!
#'
#' This function only searches the immediate children of `node` for matches,
#' and only replaces the first match. To search all descendants of `node`, or
#' to replace all matches, use [replace_nodes()] instead.
#'
#' @param node (ASTNode) A node to search for matches.
#' @param pattern (ASTNode) A node to replace.
#' @param replacement (ASTNode) A replacement for the matched node.
#' @param in_place (logical) If `FALSE`, parent of `node` is copied before
#' replacement.
#' @param ... Additional arguments to [match_object()].
#'
#' @return The node after replacement.
#'
#' @seealso [replace_nodes()]
#' @examples
#' node = quote_ast(x <- a + b)
#' ab = node$read
#' replace_in(node, ab, Symbol$new("t"))
#' @export
replace_in =
function(node, pattern, replacement, in_place = TRUE, ...)
{
  m = match_object(pattern, children(node), 0L, ...)
  if (m == 0L)
    stop("Could not find pattern in node.")

  if (!in_place)
    node = copy(node)

  # TODO: This may not be the right way index if we change [[ to do
  # language-object-style subsetting.
  node[[m]] = replacement
  node
}



#' Transform Elements in a `Container` Object
#'
#' This function transforms each element in a `Container` object by applying
#' the given `transform` function.
#'
#' This is an in-place transformation!
#'
#' This function is intended for transforming lines in `Block` objects, but
#' also works with other `Container` objects.
#'
#' This function can transform a single element in a container into multiple
#' elements. If the `transform` function returns a list, the original element
#' is replaced with one element for each element in the list.
#'
#' If the `transform` function returns an ASTNode, the original element is
#' replaced with that node.
#'
#' The index of the original element is passed to the `transform` function as
#' `index`. If the top-level `node` argument was a `BlockList` object, the
#' block index is also passed to the `transform` function as `block_index`.
#'
#'
#' @param node (ASTNode) The node to transform.
#' @param transform (function) The transformation function to apply to each
#' line. See the details section.
#'
#' @return The `node` argument after transformation.
#'
#' @seealso [replace_nodes()]
#' @export
transform_lines =
function(node, transform, ...)
{
  UseMethod("transform_lines")
}


#' @export
transform_lines.BlockList =
function(node, transform, ...)
{
  for (b in seq_along(node))
    node[[b]] = transform_lines(node[[b]], transform, ..., block_index = b)

  node
}


#' @export
transform_lines.Container =
function(node, transform, ...)
{
  indices = rev(seq_along(node))

  for (i in indices) {
    new_elts = transform(node[[i]]
      , ...
      , index = i)

    node$contents = append(node$contents[-i], new_elts, i)
  }

  node
}
