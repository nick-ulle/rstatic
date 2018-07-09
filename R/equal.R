#' Test Equality of RStatic Objects
#'
#' These functions and operators allow comparison of RStatic objects.
#'
#' These functions traverse the given RStatic objects to check that their
#' children are also equal. Except for Symbol names and Literal values, fields
#' that do not contain children are ignored.
#'
#' @param x,y (ASTNode) The objects to compare.
#'
#' @return A logical scalar indicating the result of the comparison.
#'
#' @examples
#' x = code_objects_ast(a <- 3)
#' y = code_objects_ast(a <- 3)
#' z = code_objects_ast(a <- 4)
#'
#' x == y
#'
#' y == z
#' @export
equal =
function(x, y)
{
  identical(x, y) ||
    (identical(class(x), class(y)) && .equal(x, y))
}


.equal =
function(x, y)
{
  UseMethod(".equal")
}

#' @export
.equal.ASTNode =
function(x, y)
{
  children1 = children(x)
  children2 = children(y)

  for (i in seq_along(children1)) {
    if ( !equal(children1[[i]], children2[[i]]) )
      return (FALSE)
  }

  TRUE
}

#' @export
.equal.Container =
function(x, y)
{
  (nchildren(x) == nchildren(y)) && NextMethod()
}

#' @export
.equal.Symbol =
function(x, y)
{
  if (x$basename != y$basename)
    return (FALSE)

  n1 = x$ssa_number
  n2 = y$ssa_number
  (is.na(n1) && is.na(n2)) || (n1 == n2)
}

#' @export
.equal.Parameter = function(x, y)
{
  (x$default == y$default) && NextMethod()
}

#' @export
.equal.Literal = function(x, y) {
  x$value == y$value
}


#' @rdname equal
#' @export
`==.ASTNode` = equal

#' @rdname equal
#' @export
`!=.ASTNode` = function(x, y) {
  !equal(x, y)
}
