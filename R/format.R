#
# Functions for string conversion and printing.
#

# Utility functions --------------------------------------------------

.print = function(x, ...) cat(format(x, ...), "\n\n")

class_tag = function(x) sprintf("<%s>", class(x)[[1]])

deparse_to_string = function(expr, ...) {
  # Safely deparse() an R language object to a string.
  paste0(deparse(expr, ...), collapse = "\n")
}


# toString() methods ----------------------------------------

#' Convert an ASTNode to a Character String
#'
#' These methods produce a single string that describes the code an ASTNode
#' represents.
#'
#' @param x (ASTNode) The object to be converted.
#' @param ... Optional arguments passed to or from methods.
#'
#' @return A character vector of length 1.
#'
#' @examples
#' ast = quote_ast(if (x > 3) 42 else "hello")
#' toString(ast)
#' @export
toString.ASTNode =
function(x, ...) {
  deparse_to_string(to_r(x, ...))
}

#' @export
toString.Branch =
function(x, ...) {
  classname = tolower(class(x)[[1]])
  paste(classname, toString.Label(x$target, ...))
}

#' @export
toString.Return =
function(x, ...) {
  sprintf("return (%s)", toString(x$read))
}

toString.Assign =
function(x, ..., short = TRUE) {
  write = toString(x$write, ..., short = short)
  read = toString(x$read, ..., short = short)
  sprintf("%s = %s", write, read)
}


toString.FunctionBlocks =
function(x, ..., short = FALSE) {
  if (short)
    return ("function #...")

  # A list of blocks.
  code = vapply(x$blocks, format.Block, NA_character_, ..., short = TRUE)
  paste0("[[", seq_along(code), "]]\n", code, collapse = "\n\n")
}

toString.Function =
function(x, ..., short = FALSE) {
  if (short) {
    "function #..."

  } else if (is(x$body, "list")) {
    toString.FunctionBlocks(x, ..., short = short)

  } else {
    NextMethod()
  }
}

#' @export
toString.If =
function(x, ...) {
  sprintf("if (%s) %s else %s",
    toString(x$condition, ...),
    toString(x$true, ...),
    toString(x$false, ...)
  )
}

#' @export
toString.For =
function(x, ...) {
  sprintf("for (%s in %s) %s then %s",
    toString(x$variable),
    toString(x$iterator),
    toString(x$body),
    toString(x$exit)
  )
}

#' @export
toString.While =
function(x, ...) {
  # NOTE: This prints `repeat` as `while (TRUE)`
  sprintf("while (%s) %s then %s",
    toString(x$condition),
    toString(x$body),
    toString(x$exit)
  )
}

#' @rdname toString.ASTNode
#'
#' @param block_prefix (character) Prefix to prepend to basic block IDs.
#'
#' @export
toString.Label =
function(x, ..., block_prefix = "%") {
  paste0(block_prefix, x$name)
}


# format() and print() methods ----------------------------------------
#' @export
format.ASTNode = function(x, ...) {
  members = setdiff(ls(x), c("initialize", "clone"))
  is_method = vapply(members, function(f) is.function(.subset2(x, f)), NA)

  members[is_method] = paste(members[is_method], "()", sep = "")
  members = members[order(is_method, members)]
  members = paste("$", members, sep = "", collapse = " ")

  #if (is(x, "Function") && is(x$body, "list")) {
  #  code = vapply(x$body, format, NA_character_)
  #  code = paste0("[[", seq_along(code), "]]\n", code, collapse = "\n")
  #} else
    code = toString(x, ...)

  sprintf("%s %s\n%s", class_tag(x), members, code)
}

#' @export
print.ASTNode = .print


# REMOVE ----------------------------------------

format.Block =
function(x, ..., short = TRUE) {
  len = length(x$body)
  if (len == 0)
    return ("\n  # empty block")

  phi = vapply(x$phi, toString, NA_character_, ...)
  body = vapply(x$body, toString, NA_character_, ..., short = short)

  paste("\n  ", c(phi, body), sep = "", collapse = "")
}

#' @export
print.Block = .print

print.BlocksList =
function(x, ...) {
  print(x$body)
}

#' @export
#format.FlowGraph = function(x, tag = .format_tag(x), ...) {
#  # Format:
#  #
#  #   <CFGraph> 5 blocks
#  #
#  #   %v1:
#  #     # branch %2
#  #
#  #   %v2
#  #     if (z > 3) %3 else %4
#
#  msg = if (length(x) == 1) "%i block" else "%i blocks"
#  v_count = sprintf(msg, length(x))
#
#  fmt = vapply(x$blocks, format, NA_character_, keep_ssa = TRUE)
#  blocks = sprintf('%s: %s', names(x), fmt)
#  blocks = paste0(blocks, collapse = "\n\n")
#
#  sprintf("%s %s\n\n%s", tag, v_count, blocks)
#}
#
##' @export
#print.FlowGraph = .print
#
#
#format.NONCONST = function(x, ...) "NONCONST"
#print.NONCONST = function(x, ...) cat(format(x, ...), "\n")
