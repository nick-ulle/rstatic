`==.ASTNode` = function(e1, e2) {
  if (!identical(class(e1), class(e2)))
    FALSE
  else
    are_equal(e1, e2)
}

`!=.ASTNode` = function(e1, e2) {
  !`==.ASTNode`(e1, e2)
}


are_equal = function(e1, e2) {
  UseMethod("are_equal")
}


# Invocations
# --------------------
are_equal.Invocation = function(e1, e2) {
  if (length(e1$args) != length(e2$args))
    return (FALSE)

  args_equal = as.logical(Map(`==`, e1$args, e2$args))
  all(args_equal)
}

are_equal.Call = function(e1, e2) {
  (e1$fn == e2$fn) && NextMethod()
}

# Assignments
# --------------------
are_equal.Assign = function(e1, e2) {
  (e1$read == e2$read) && (e1$write == e2$write)
}


# Symbols
# --------------------
are_equal.Symbol = function(e1, e2) {
  n1 = e1$ssa_number
  n2 = e2$ssa_number

  (e1$basename == e2$basename) &&
  ((is.na(n1) && is.na(n2)) || (n1 == n2))
}

are_equal.Parameter = function(e1, e2) {
  (e1$default == e2$default) && NextMethod()
}


# Literals
# --------------------
are_equal.Literal = function(e1, e2) {
  e1$value == e2$value
}
