
#' Fields Containing Children for an RStatic Object
#'
#' Given an RStatic object, this function returns the names of fields which
#' contain children.
#'
#' This function is the basis for most of the functions that extract or replace
#' children from RStatic objects.
#'
#' @param x (ASTNode) The node.
#'
#' @seealso \code{ls} to get the names of all fields.
#'
#' @export
child_fields = function(x) UseMethod("child_fields")

#' @export
child_fields.Branch = function(x) c("target")


#' @export
child_fields.Return = function(x) c("target", "read")

#' @export
child_fields.If    = function(x) c("true", "false", "condition")
#' @export
child_fields.Loop  = function(x) c("body", "exit")
#' @export
child_fields.While = function(x) c("body", "exit", "condition")
#' @export
child_fields.For   = function(x) c("body", "exit", "variable", "iterator")

#' @export
child_fields.Invocation = function(x) c("args")
#' @export
child_fields.Call       = function(x) c("fn", "args")

#' @export
child_fields.Assign    = function(x) c("write", "read")
#' @export
child_fields.Parameter = function(x) c("default")
#' @export
child_fields.Function  = function(x) c("body", "params")

#' @export
child_fields.ASTNode = function(x) character(0)

# nchildren ------------------------------------------------------------

#' The Number of Children of an RStatic Object
#'
#' This function returns the number of children of the given node.
#'
#' @param x (ASTNode) The node.
#'
#' @export
nchildren =
function(x)
{
  UseMethod("nchildren")
}

#' @export
nchildren.ASTNode = function(x) {
  length(child_fields(x))
}

#' @export
nchildren.Container = function(x) {
  length(x$contents)
}


# ------------------------------------------------------------

#' Extract or Replace Descendant of an RStatic Object
#'
#' This function extracts or replaces a descendant of the given node.
#'
#' If \code{index} is a vector, the \code{child} function is called recursively
#' so that descendants several levels below \code{node} are accessible.
#'
#'
#' @param node (ASTNode) The node from which to extract or replace the
#' descendant.
#' @param index (integer) The index of the descendant to extract or replace.
#' @param value (ASTNode) The value to replace the descendant with.
#'
#' @return A single child node.
#'
#' @seealso \code{children} to extract or replace all children at once.
#' @examples
#' code = quote_ast(mean(1, 2, 3 + x))
#' x = child(code, c(2, 3, 2, 2))
#'
#' child(code, c(2, 3, 2, 2)) = Integer$new(4)
#' @export
child =
function(node, index)
{
  UseMethod("child")
}

#' @export
child.ASTNode =
function(node, index)
{
  len = length(index)
  if (len == 0)
    return (node)

  fields = child_fields(node)
  if (len == 1)
    return (.subset2(node, fields[[index]]))

  field = fields[[ index[[1L]] ]]
  child(.subset2(node, field), index[-1L])
}

#' @export
child.Container =
function(node, index)
{
  len = length(index)
  if (len == 0)
    return (node)
  if (len == 1)
    return (.subset2(node$contents, index))

  i = index[[1L]]
  child(.subset2(node$contents, i), index[-1L])
}


#' @rdname child
#' @export
`child<-` =
function(node, index, value)
{
  len = length(index)
  if (len == 0)
    stop("'index' must have length at least 1.")

  if (len == 1) {
    set_immediate_child(node, index, value)

  } else {
    parent = child(node, index[-len])
    set_immediate_child(parent, index[[len]], value)
  }

  invisible(node)
}


set_immediate_child =
function(node, index, value)
{
  UseMethod("set_immediate_child")
}

#' @export
set_immediate_child.ASTNode =
function(node, index, value)
{
  assign(child_fields(node)[[index]], value, envir = node)
}

#' @export
set_immediate_child.Container =
function(node, index, value)
{
  node$contents[[index]] = value
}

#' Extract or Replace All Children of an RStatic Object
#'
#' This function extracts or replaces all children of the given node.
#'
#' @param node (ASTNode) The node from which to extract or replace the
#' children.
#' @param value (list) A named list of objects to replace the children with.
#' Names should match field names of \code{node}.
#'
#' @return A list of the child nodes.
#'
#' @seealso \code{child} to extract or replace a single, specific descendant
#' slightly more efficiently.
#' @examples
#' code = quote_ast(x <- 3)
#' children(code)
#'
#' children(code) = list(read = Integer$new(42), write = Symbol$new("y"))
#' @export
children =
function(node)
{
  UseMethod("children")
}

#' @export
children.ASTNode =
function(node)
{
  mget(child_fields(node), node)
}

#' @export
children.Container =
function(node)
{
  node$contents
}

#' @export
children.list =
function(node)
{
  node
}


#' @rdname children
#' @export
`children<-` =
function(node, value)
{
  UseMethod("children<-")
}

#' @export
`children<-.ASTNode` =
function(node, value) {
  names = names(value)
  if (!is.list(value) || is.null(names))
    stop("Assigned 'value' must be a named list", call. = FALSE)

  invalid = match(names, child_fields(node), 0L) == 0L
  if (any(invalid)) {
    msg = sprintf("Object of class '%s' does not have fields %s.",
      class(node)[[1]], paste0("'", names[invalid], "'", collapse = ", "))
    stop(msg, call. = FALSE)
  }

  # We can't tell which fields actually changed, so be conservative and set all
  # of them.
  mapply(assign, names, value, MoreArgs = list(envir = node),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  node
}

#' @export
`children<-.Container` =
function(node, value)
{
  if (!is.list(value))
    stop("Assigned 'value' must be a list", call. = FALSE)

  node$contents = value
  node
}

#' @export
`children<-.list` =
function(node, value)
{
  modifyList(node, value, keep.null = TRUE)
}
