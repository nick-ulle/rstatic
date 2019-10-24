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
