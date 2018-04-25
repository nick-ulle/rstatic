#' Insert Optional Return Expressions
#'
#' This function transforms a function so that every expression that can be
#' returned is explicitly marked by a \code{return()} expression.
#'
#' This is an in-place transformation! This function may not behave as expected
#' if called on a non-function.
#' 
#' @param node (Function) The function to transform.
#' @param ... Optional arguments to and from methods.
#' @param recursive (logical) Should functions defined in \code{node} also be
#' transformed?
#'
#' @export
insert_return =
function(node, ..., recursive = TRUE) {
  UseMethod("insert_return")
}

#' @export
insert_return.Function =
function(node, ..., recursive = TRUE, .first = TRUE) {
  if (recursive || .first) {
    node$body = insert_return(node$body, ..., recursive = recursive,
      .first = FALSE)
  }

  node
}

#' @export
insert_return.Brace =
function(node, ..., recursive = TRUE) {
  len = length(node$body)

  # Empty brace.
  if (len == 0L) {
    node$body[[1L]] = Return$new(Null$new(), parent = node)
    return (node)
  }

  # Check for function definitions.
  if (recursive) {
    fns = find_functions(node$body[-len])
    lapply(fns, insert_return.Function, ..., recursive = recursive)
  }

  last = insert_return(node$body[[len]], ..., recursive = recursive)

  node$body = c(node$body[-len], last)

  node
}

#' @export
insert_return.If =
function(node, ...) {
  node$true = insert_return(node$true, ...)
  node$false = insert_return(node$false, ...)

  node
}

#' @export
insert_return.Loop =
function(node, ...) {
  # Need to insert a return(NULL) on following line
  list(
    node,
    Return$new(Null$new())
  )
}


#' @export
insert_return.Assign =
function(node, ...) {
  list(
    node,
    Return$new(node$write$copy())
  )
}


#' @export
insert_return.Literal =
function(node, ...) {
  Return$new(node)
}

#' @export
insert_return.Symbol = insert_return.Literal

#' @export
insert_return.Application = insert_return.Literal


#' @export
insert_return.Branch =
function(node, ...) {
  node
}
