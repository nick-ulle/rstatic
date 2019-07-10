#' Get Index Arguments From Subset Call
#'
#' This function gets the index arguments from a call to a subset function.
#'
#' @param node (Subset) A call to a subset function.
#'
#' @return A list of index arguments.
#'
#' @export
get_index = function(node) {
  UseMethod("get_index")
}


#' @export
get_index.Subset1 = function(node) {
  # First argument is always the object.
  args = node$args$contents[-1L]

  # If last argument is called "drop", remove it as well.
  idx = match("drop", names(args), 0L)
  if (idx != 0L)
    args = args[-idx]

  args
}


#' @export
get_index.Subset2 = function(node) {
  # First argument is always the object.
  args = node$args$contents[-1L]

  # If last argument is called "exact", remove it as well.
  idx = match("exact", names(args), 0L)
  if (idx != 0L)
    args = args[-idx]

  args
}


#' @export
get_index.SubsetDollar = function(node) {
  # First argument is always the object.
  args = node$args$contents[-1L]

  args
}
