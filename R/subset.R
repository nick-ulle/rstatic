
fieldname = function(i, x) UseMethod("fieldname", x)

fieldname.Branch   = function(i, x) c("target")[[i]]
  fieldname.Return = function(i, x) c("target", "read")[[i]]

fieldname.If      = function(i, x) c("true", "false", "condition")[[i]]
fieldname.Loop    = function(i, x) c("body", "exit")[[i]]
  fieldname.While = function(i, x) c("body", "exit", "condition")[[i]]
  fieldname.For   = function(i, x) c("body", "exit", "variable", "iterator")[[i]]

fieldname.Application = function(i, x) c("args")[[i]]
  fieldname.Call      = function(i, x) c("fn", "args")[[i]]

fieldname.Assign    = function(i, x) c("read", "write")[[i]]
fieldname.Parameter = function(i, x) c("default")[[i]]
fieldname.Function  = function(i, x) c("body", "params")[[i]]

fieldname.default = function(i, x) {
  msg = sprintf("No field mapping defined for '%s'.", toString(class(x)))
  stop(msg)
}

# Length ----------------------------------------

#' @export
length.Container = function(x) {
  length(x$body)
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

  x = .subset2(x, fieldname(i[[1L]], x), ...)

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

  f = fieldname(i[[1L]], x)

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
