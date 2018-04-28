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
  length(x$body)
}

#' @export
length.FunctionBlocks = function(x) {
  length(x$blocks)
}


# Subset ----------------------------------------

#' @export
`[.Container` = function(x, i, ...) {
  x$body[i, ...]
}


# Subset2 ----------------------------------------

#' @export
`[[.ASTNode` = function(x, i, ...) {
  if (!is.numeric(i))
    return (NextMethod())

  x = .subset2(x, names(x)[[ i[[1L]] ]], ...)

  if (length(i) > 1)
    x[[ i[-1L], ... ]]
  else
    x
}

#' @export
`[[.Container` = function(x, i, ...) {
  if (!is.numeric(i))
    return (NextMethod())

  x = x$body[[ i[[1L]], ... ]]

  if (length(i) > 1)
    x[[ i[-1], ... ]]
  else
    x
}

#' @export
`[[.FunctionBlocks` = function(x, i, ...) {
  if (!is.numeric(i))
    return (NextMethod())

  x = x$blocks[[ i[[1L]], ... ]]

  if (length(i) > 1)
    x[[ i[-1], ... ]]
  else
    x
}



# Replacement2 ----------------------------------------

#' @export
`[[<-.ASTNode` = function(x, i, ..., value) {
  if (!is.numeric(i))
    return (NextMethod())

  f = names(x)[[ i[[1L]] ]]

  if (length(i) > 1)
    # Call the replacement method for the child node.
    value = `[[<-`(x[[f]], i[-1L], ..., value = value)

  i = f
  # Replace, ignoring the ASTNode class.
  NextMethod()
}

#' @export
`[[<-.Container` = function(x, i, ..., value) {
  if (!is.numeric(i))
    return (NextMethod())

  f = i[[1L]]

  if (length(i) > 1)
    value = `[[<-`(x$body[[f]], i[-1L], ..., value = value)

  # Replace the list element.
  x$body[[f, ...]] = value
  x
}

#' @export
`[[<-.FunctionBlocks` = function(x, i, ..., value) {
  if (!is.numeric(i))
    return (NextMethod())

  f = i[[1L]]

  if (length(i) > 1)
    value = `[[<-`(x$blocks[[f]], i[-1L], ..., value = value)

  # Replace the list element.
  x$blocks[[f, ...]] = value
  x
}
