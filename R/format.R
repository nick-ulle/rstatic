#
# Functions for string conversion and printing.
#

.print = function(x, ...) cat(format(x, ...), "\n\n")

.format_tag = function(x) sprintf("<%s>", class(x)[1])


#' @export
format.ASTNode = function(x, indent = 0, ...) {
  members = setdiff(ls(x), c("initialize", "clone"))
  is_method = vapply(members, function(f) is.function(x[[f]]), TRUE)

  members[is_method] = paste(members[is_method], "()", sep = "")
  members = members[order(is_method, members)]
  members = paste("$", members, sep = "", collapse = " ")

  code = deparse_to_string(to_r(x, ...))
  sprintf("<%s> %s\n%s", class(x)[1], members, code)
}

#' @export
print.ASTNode = .print

format.Block = function(x, ...) {
  len = length(x$body)
  if (len == 0)
    return ("\n  # empty block")

  body = x$body
  #last = x$body[[len]]
  #if (is(last, "If"))
  #  last = sprintf("if (%s) %s else %s",
  #    deparse_to_string(to_r(last$condition, ...)),
  #    last$true,
  #    last$false
  #  )

  #body = x$body[-len]

  fmt = vapply(body, function(line) deparse_to_string(
      to_r(line, use_inner_blocks = TRUE)), "")

  paste("\n  ", fmt, sep = "", collapse = "")
}

#' @export
print.Block = .print

#' @export
format.FlowGraph = function(x, tag = .format_tag(x), ...) {
  # Format:
  #
  #   <CFGraph> 5 blocks
  #
  #   %v1:
  #     # branch %2
  #
  #   %v2
  #     if (z > 3) %3 else %4

  msg = if (length(x) == 1) "%i block" else "%i blocks"
  v_count = sprintf(msg, length(x))

  fmt = vapply(x$blocks, format, NA_character_ , use_inner_blocks = FALSE)
  blocks = sprintf('%s: %s', names(x), fmt)
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
