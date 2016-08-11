#
# Functions for string conversion and printing.
#

print.ASTNode = function(object) {
  # TODO: print field names.
  cat(sprintf("<%s>\n", class(object)[1]))
  print(to_r(object))
}

