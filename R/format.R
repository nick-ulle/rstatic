#
# Functions for string conversion and printing.
#

.print = function(x, ...) cat(format(x, ...), "\n\n")

.format_tag = function(x) sprintf("<%s>", class(x)[1])


#' @export
format.ASTNode = function(x, indent = 0, ...) {
  members = setdiff(ls(x), c("initialize", "clone"))
  oclass = class(x)
  class(x) = c("ASTNode", "R6")
  #  is_method = vapply(members, function(f) is.function(base::`[[`(x, f)), TRUE)
  is_method = vapply(members, function(f) is.function(x[[f]]), TRUE)  
  class(x) = oclass
  
  members[is_method] = paste(members[is_method], "()", sep = "")
  members = members[order(is_method, members)]
  members = paste("$", members, sep = "", collapse = " ")

  code = deparse_to_string(to_r(x, ...))
  sprintf("<%s> %s\n%s", class(x)[1], members, code)
}

#' @export
print.ASTNode = .print

#' @export
format.FlowGraph = function(x, tag = .format_tag(x), ...) {
  # Format:
  #
  #   <CFGraph> 5 blocks
  #
  #   %v1 <BasicBlock>
  #   # branch %2
  #
  #   %v2 <BasicBlock>
  #   # if (z > 3) %3 else %4

  msg = if (length(x) == 1) "%i block" else "%i blocks"
  v_count = sprintf(msg, length(x))

  fmt = vapply(x$blocks, format, NA_character_ , use_inner_blocks = FALSE)
  blocks = sprintf('%s %s', names(x), fmt)
  blocks = paste0(blocks, collapse = "\n\n")

  sprintf("%s %s\n\n%s", tag, v_count, blocks)
}

#' @export
print.FlowGraph = .print


format.NONCONST = function(x, ...) "NONCONST"
print.NONCONST = function(x, ...) cat(format(x, ...), "\n")


deparse_to_string = function(expr, ...) {
  # Safely deparse() to a single string.
  paste0(deparse(expr, ...), collapse = "\n")
}
