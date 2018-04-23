insert_after = function(code, i, line) {
  idx = seq(i, nrow(code))
  code[idx + 1, ] = code[idx, ]

  #code$line[i] = list(line) # Works
  #code[i, "line"] = list(list(line)) # Works
  code[[i, "line"]] = line

  code
}


#`[.CodeList` = function(x, i, ...) {
#  if (length(i) == 1)
#    .subset2(x, i, ...)
#  else
#    .subset(x, i, ...)
#}


#`[<-.CodeList` = function(x, i, ..., value) {
#  browser()
#  if (length(i) == 1) {
#    cx = oldClass(x)
#
#    class(x) = NULL
#    x[i, ...] = list(value)
#    class(x) = cx
#
#    x
#  } else {
#    NextMethod()
#  }
#}


block_heads = function(blocks) {
  which(blocks != c("", head(blocks, -1)))
}

block_tails = function(blocks) {
  which(blocks != c(tail(blocks, -1), ""))
}
