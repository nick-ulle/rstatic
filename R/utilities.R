`:=` = function(x, y) {
  x = substitute(x)
  if (length(x) < 2 || x[[1]] != "c")
    stop("left-hand side of unpack must be c(...).")
  x = x[-1]

  is_name = vapply(x, is.name, TRUE)
  if (!all(is_name))
    stop("cannot unpack into non-variables.")
  x = as.character(x)

  len_x = length(x)
  len_y = length(y)
  if (len_x != len_y)
    stop(sprintf("cannot unpack %i elements into %i variables.", len_y, len_x))

  keep = x != ""
  mapply(assign, x[keep], y[keep], MoreArgs = list(envir = parent.frame()),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  invisible(NULL)
}
