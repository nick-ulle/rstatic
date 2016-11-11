#
# Functions for string conversion and printing.
#

.print = function(x, ...) cat(format(x, ...))


#' @export
format.ASTNode = function(x, indent = 0, ...) {
  fields = paste("$", ls(x), sep = "", collapse = " ")
  code = paste0(deparse(to_r(x)), collapse = "\n")
  sprintf("<%s> %s\n%s\n", class(x)[1], fields, code)
}

#' @export
print.ASTNode = .print


#' @export
format.CFGraph = function(x, ...) {
  sprintf("<%s>\n$entry_block\n%s\n", class(x)[1], format(x$entry_block))
}

#' @export
print.CFGraph = .print


#' @export
format.BasicBlock = function(x, ...) {
  # FIXME: Indent the code.
  # FIXME: Print the terminator.
  code = paste0(lapply(x$body, format), collapse = "\n")
  sprintf("<%s>\n%s", class(x)[1], code)
}

#' @export
print.BasicBlock = .print
