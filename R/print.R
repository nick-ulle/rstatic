#
# Functions for string conversion and printing.
#

#' @export
print.ASTNode = function(object) {
  fields = paste("$", ls(object), sep = "", collapse = " ")
  cat(sprintf("<%s> %s\n", class(object)[1], fields))
  print(to_r(object))
}

