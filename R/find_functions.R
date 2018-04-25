#' Find Function Definitions in a Code Object
#'
#' This function takes a code object and returns a list of all function
#' definitions.
#'
#' @param node (ASTNode) The code object to search.
#' @param initial (list) An initial list of functions.
#' @param ... Additional arguments to and from methods.
#' @param recursive (logical) Should function definitions in \code{node} also
#' be searched?
#'
#' @export
find_functions =
function(node, initial = list(), ..., recursive = FALSE) {
  UseMethod("find_functions")
}


#' @export
find_functions.list =
function(node, initial = list(), ...) {
  for (l in node)
    initial = find_functions(l, initial, ...)

  initial
}


#' @export
find_functions.Container =
function(node, initial = list(), ...) {
  for (l in node$body)
    initial = find_functions(l, initial, ...)

  initial
}

#' @export
find_functions.If =
function(node, initial = list(), ...) {
  initial = find_functions(node$true, initial, ...)
  find_functions(node$false, initial, ...)
}

#' @export
find_functions.Loop =
function(node, initial = list(), ...) {
  # NOTE: Does anyone put functions in loop conditions?
  initial = find_functions(node$body, initial, ...)
}

#' @export
find_functions.Assign =
function(node, initial = list(), ...) {
  initial = find_functions(node$read, initial, ...)
  find_functions(node$write, initial, ...)
}

#' @export
find_functions.Function =
function(node, initial = list(), ..., recursive = FALSE) {
  initial = c(initial, node)

  if (recursive)
    initial = find_functions(node$body, initial, ..., recursive)

  initial
}

#' @export
find_functions.ASTNode =
function(node, initial = list(), ...) {
  initial
}
