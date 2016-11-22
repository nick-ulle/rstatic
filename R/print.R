#
# Functions for string conversion and printing.
#

.print = function(x, ...) cat(format(x, ...), "\n\n")

.format_tag = function(x) sprintf("<%s>", class(x)[1])


#' @export
format.ASTNode = function(x, indent = 0, ...) {
  fields = paste("$", ls(x), sep = "", collapse = " ")
  code = paste0(deparse(to_r(x)), collapse = "\n")
  sprintf("<%s> %s\n%s", class(x)[1], fields, code)
}

#' @export
print.ASTNode = .print


#' @export
format.CFGraph = function(x, ...) {
  # Format:
  #
  #   <CFGraph> 5 vertices
  #   [[1]]
  #   <BasicBlock>
  #   # branch %2
  #
  #   [[2]]
  #   <BasicBlock>
  #   # if (z > 3) %3 else %4

  tag = .format_tag(x)
  if (x$len == 1)
    v_count = sprintf("%i block", x$len)
  else
    v_count = sprintf("%i blocks", x$len)

  fmt = vapply(x$blocks, format, character(1), show_body = FALSE)
  blocks = sprintf("[[%i]]\n%s", seq_along(x$blocks), fmt)
  blocks = paste0(blocks, collapse = "\n\n")

  sprintf("%s %s\n%s", tag, v_count, blocks)
}

#' @export
print.CFGraph = .print


#' @export
format.BasicBlock = function(x, show_body = TRUE, ...) {
  # Format:
  #
  #   <BasicBlock>
  #   x = x + 1
  #   x = x / y
  #   foo(x)
  #   # if (x < 4) %3 else %4
  #
  terminator = format(x$terminator, show_tag = FALSE)

  if (show_body) {
    body = vapply(x$body, function(.) deparse(to_r(.)), character(1))
    body = paste0(body, collapse = "\n")

    msg = sprintf("%s\n%s\n# %s", .format_tag(x), body, terminator)

  } else
    msg = sprintf("%s\n# %s", .format_tag(x), terminator)

  return (msg)
}

#' @export
print.BasicBlock = .print


#' @export
format.BranchInst = function(x, show_tag = TRUE, ...) {
  if (is.null(x$condition)) {
    term = sprintf("branch %%%s", x$true)
  } else {
    condition = deparse(to_r(x$condition))
    term = sprintf("branch (%s) %%%i, %%%i", condition, x$true, x$false)
  }

  if (show_tag)
    msg = sprintf("%s\n%s", .format_tag(x), term)
  else 
    msg = sprintf("%s", term)

  return (msg)
}

#' @export
print.BranchInst = .print


#' @export
format.IterateInst = function(x, show_tag = TRUE, ...) {

  ivar = deparse(to_r(x$ivar))
  iter = deparse(to_r(x$iter))
  term = sprintf("iterate (%s in %s) %%%i, %%%i", ivar, iter, x$body, x$exit)

  if (show_tag)
    msg = sprintf("%s\n%s", .format_tag(x), term)
  else
    msg = sprintf("%s", term)

  return (msg)
}
