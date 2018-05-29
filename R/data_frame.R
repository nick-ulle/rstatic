#insert_after = function(code, i, line) {
#  idx = seq(i, nrow(code))
#  code[idx + 1, ] = code[idx, ]
#
#  #code$line[i] = list(line) # Works
#  #code[i, "line"] = list(list(line)) # Works
#  code[[i, "line"]] = line
#
#  code
#}
#
#block_heads = function(blocks) {
#  which(blocks != c("", head(blocks, -1)))
#}
#
#block_tails = function(blocks) {
#  which(blocks != c(tail(blocks, -1), ""))
#}
#
#
##' @rdname as_data_frame
##'
##' @export
#as.data.frame.BlockList =
#function(x, ...) {
#  blocks = x$contents
#  lens = vapply(blocks, length, 0L)
#  ids = rep(seq_along(blocks), lens)
#
#  depths = vapply(blocks, function(b) b$depth, 0L)
#  depths = rep(depths, lens)
#
#  lines = lapply(blocks, function(block) block$contents)
#  lines = unlist(lines, recursive = FALSE, use.names = FALSE)
#  class(lines) = "CodeList"
#
#  data.frame(line = I(lines), block = ids, depth = depths)
#}
#
##' @rdname as_data_frame
##'
##' @export
#as.data.frame.Function =
#function(x, ...) {
#  as.data.frame(x$body)
#}
#
##' Convert Basic Blocks to a Code Data Frame
##'
##' This function converts basic blocks to a data frame where each row is one
##' ``line'' of code.
##'
##' @param x (BlockList) The basic blocks to convert.
##' @param ... Additional arguments to be passed to or from methods.
##'
##' @export
#as_data_frame = as.data.frame
#
#
##' Convert Code Data Frame to Basic Blocks
##'
##' This function converts a code data frame to basic blocks.
##'
##' @param code (data.frame) The data frame to convert.
##' @param new_blocks (logical) Whether to create new Block objects. Beware that
##' these new blocks will become the new parents of the expressions in the
##' block.
##'
##' @export
#as_blocks = function(code, new_blocks = FALSE) {
#  heads = block_heads(code[["block"]])
#
#  if (new_blocks) {
#    blocks = split(code, code$block)
#    blocks = lapply(blocks, function(b) {
#      Block$new(unclass(b[["line"]]), b[[1, "id"]], b[[1, "depth"]])
#    })
#    idx = as.numeric(names(blocks))
#
#  } else {
#    blocks = lapply(heads, function(i) code[[i, "line"]]$parent)
#    idx = code[heads, "block"]
#  }
#
#  result = list()
#  result[idx] = blocks
#
#  result
#}
