# To see the fields, use `ls(..., all = T)`

#' @export
names.Branch   = function(x) c("target")
#' @export
  names.Return = function(x) c("target", "read")

#' @export
names.If      = function(x) c("true", "false", "condition")
#' @export
names.Loop    = function(x) c("body", "exit")
#' @export
  names.While = function(x) c("body", "exit", "condition")
#' @export
  names.For   = function(x) c("body", "exit", "variable", "iterator")

#' @export
names.Application = function(x) c("args")
#' @export
  names.Call      = function(x) c("fn", "args")

#' @export
names.Assign    = function(x) c("write", "read")
#' @export
names.Parameter = function(x) c("default")
#' @export
names.Function  = function(x) c("body", "params")

#' @export
names.ASTNode = function(x) character(0)


# Length ----------------------------------------

#' @export
length.ASTNode = function(x) {
  length(names(x))
}

#' @export
length.Container = function(x) {
  length(x$contents)
}


# Subset ----------------------------------------

#' @export
`[.Container` = function(x, i, ...) {
  x$contents[i, ...]
}


# Subset2 ----------------------------------------

#' @export
`[[.ASTNode` = function(x, i, ...) {
  if (!is.numeric(i))
    return (NextMethod())

  x = .subset2(x, names(x)[[ i[[1L]] ]], ...)

  if (length(i) <= 1)
    return (x)

  x[[ i[-1L], ... ]]
}

#' @export
`[[.Container` = function(x, i, ...) {
  if (!is.numeric(i))
    return (NextMethod())

  x = x$contents[[ i[[1L]], ... ]]

  if (length(i) <= 1)
    return (x)

  x[[ i[-1], ... ]]
}


# Replacement2 ----------------------------------------

#' @export
`[[<-.ASTNode` = function(x, i, ..., value) {
  if (!is.numeric(i))
    return (NextMethod())

  field = names(x)[[ i[[1L]] ]]

  if (length(i) > 1)
    # Call the replacement method for the child node.
    value = `[[<-`(x[[field]], i[-1L], ..., value = value)

  i = field
  # Replace, ignoring the ASTNode class.
  NextMethod()
}

#' @export
`[[<-.Container` = function(x, i, ..., value) {
  if (!is.numeric(i))
    return (NextMethod())

  i1 = i[[1L]]

  if (length(i) > 1)
    value = `[[<-`(x$contents[[i1]], i[-1L], ..., value = value)

  # Replace the list element.
  x$contents[[i1, ...]] = value
  x
}
