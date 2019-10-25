#' Replace a Node in its Parent Node
#'
#' This function replaces an ASTNode object in its parent expression with
#' another ASTNode object.
#'
#' With the default arguments, this is an in-place transformation and has side
#' effects!
#'
#' Only the first node `identical()` to `node` is replaced.
#'
#' @param node (ASTNode) The node to replace.
#' @param replacement (ASTNode) The replacement node.
#' @param in_place (logical) If `FALSE`, parent of `node` is copied before
#' replacement.
#'
#' @return The parent node, after replacement.
#'
#' @examples
#' node = quote_ast(x <- a + b)
#' ab = node$read
#' replace_in_parent(node, Symbol$new("t"))
#' @export
replace_in_parent =
function(node, replacement, in_place = TRUE)
{
  parent = node$parent

  # Find the node in its parent.
  m = match_object(node, children(parent), 0L)
  if (m == 0L)
    stop("Could not find node in parent.")

  if (!in_place)
    parent = copy(parent)

  # TODO: This may not be the right way index if we change [[ to do
  # language-object-style subsetting.
  parent[[m]] = replacement
  parent
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
