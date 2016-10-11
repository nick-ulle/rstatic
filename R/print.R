#
# Functions for string conversion and printing.
#

#' @export
print.ASTNode = function(x, ...) {
  fields = paste("$", ls(x), sep = "", collapse = " ")
  cat(sprintf("<%s> %s\n", class(x)[1], fields))
  print(to_r(x))
}

