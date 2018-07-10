#' Copy an RStatic Object
#'
#' This function copies the given RStatic object, while ensuring that
#' parent-child relationships are preserved for the copied object.
#'
#' If \code{x} is any other R6 object, \code{x} is deep-cloned. If \code{x} is
#' not an R6 object, no action is taken and \code{x} is returned.
#'
#' Since RStatic objects are references, assignment does not make a copy. This
#' function can be used to explicitly copy an RStatic object.
#'
#' @param x The object to copy.
#' @param ... Additional arguments to methods.
#' @param skip_set_parent (character) Names of fields which should never have
#' \code{set_parent} called on them.
#'
#' @seealso \code{set_parent}, which is used by this function.
#' @examples
#' x = quote_ast(x <- "Hi DTL!")
#' y = x
#' z = copy(x)
#'
#' x$read = Numeric$new(141)
#'
#' # Changing 'x' changed 'y' (a reference), but not 'z' (a copy).
#' @export
copy = function(x, ...) UseMethod("copy")

#' @export
copy.ASTNode =
function(x, ..., skip_set_parent = c("parent", ".__enclose_env__")) {
  cloned = x$clone(deep = TRUE)

  # Set $parent on RStatic objects in this object's fields.
  fields = setdiff(ls(cloned, all.names = TRUE), skip_set_parent)
  is_active = vapply(fields, bindingIsActive, NA, cloned)

  items = mget(fields[!is_active], cloned)
  lapply(items, set_parent, cloned)

  cloned
}

#' @export
copy.list    = function(x, ...) lapply(x, copy)
#' @export
copy.R6      = function(x, ...) x$clone(deep = TRUE)
#' @export
copy.default = function(x, ...) x


#' Set Parent of an RStatic Object
#'
#' This function sets the parent field for the given RStatic object or list of
#' RStatic objects.
#'
#' If \code{node} is an RStatic object, then \code{set_parent(node, parent)} is
#' equivalent to \code{node$parent = parent}.
#'
#' If \code{node} is a list, \code{set_parent} is called on each element.
#'
#' @param node The object on which to set the parent.
#' @param parent The new parent for \code{node}.
#'
#' @export
set_parent = function(node, parent)
  UseMethod("set_parent")

#' @export
set_parent.ASTNode = function(node, parent) {
  node$parent = parent
  node
}

#' @export
set_parent.list = function(node, parent) {
  lapply(node, set_parent, parent)
}

#' @export
set_parent.default = function(node, parent)
  node
