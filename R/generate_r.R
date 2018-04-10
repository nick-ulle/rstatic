# FIXME: generate_r() probably doesn't generate code for nested functions.

# #' @export
generate_r = function(node, ...) {
  UseMethod("generate_r")
}


#' @export
generate_r.ControlFlowGraph = function(node, ...) {
  c(exp, ) := generate_r.Block(node[[node$entry]], cfg = node, ...)

  as.call(append(as.symbol("{"), exp))
}


#' @export
generate_r.Block = function(node, cfg, ...) {
  # Rebuild all but last line.
  len = length(node$body)
  lines = lapply(node$body[-len], to_r, ...)

  last = node$body[[len]]

  # Rebuild last line.
  succ = NA
  if (is(last, "ControlFlow")) {
    c(line, succ) := generate_r(last, cfg, ...)

  } else if (is(last, "Return")) {
    line = to_r.Return(last, ...)

  } else {
    line = to_r(last, ...)
    succ = successors(node$id, cfg)
  }

  lines = append(lines, line)
  if (is.na(succ))
    return (list(exp = lines, succ = succ))

  # Rebuild blocks until there's a depth change.
  next_block = cfg[[succ]]
  if (node$depth != next_block$depth)
    return (list(exp = lines, succ = succ))

  c(next_lines, succ) := generate_r.Block(next_block, cfg, ...)
  lines = append(lines, next_lines)

  list(exp = lines, succ = succ)
}


# Helper function to generate a braced expression from a Block.
generate_r_brace = function(id, cfg, ...) {
  gen = generate_r.Block(cfg[[id]], cfg, ...)
  lines = append(as.symbol("{"), gen[[1]])
  gen[[1]] = as.call(lines)

  gen
}


#' @export
generate_r.If = function(node, cfg, ...) {
  # Rebuild the true branch.
  c(true, succ)   := generate_r_brace(node$true, cfg, ...)
  c(false, succ2) := generate_r_brace(node$false, cfg, ...)

  # Return successor that's not caused by return/break/next.
  if (is.na(succ))
    succ = succ2
  else if (is.na(succ2))
    succ = succ
  else if (succ != succ2)
    stop("if-statement has successor conflict.")

  condition = to_r(node$condition, ...)

  # Assemble into an if-statement.
  exp = call("if", condition, true, false)

  list(exp = exp, succ = succ)
}


#' @export
generate_r.For = function(node, cfg, ...) {
  c(body, ) := generate_r_brace(node$body, cfg, ...)
  variable = to_r.Symbol(node$ivar, ...)
  iterator = to_r(node$iter, ...)

  exp = call("for", variable, iterator, body)

  list(exp = exp, succ = node$exit)
}


#' @export
generate_r.While =
function(node, cfg, ...) {
  c(body, ) := generate_r_brace(node$body, cfg, ...)

  exp =
    if (node$is_repeat) {
      call("repeat", body)
    } else {
      condition = to_r(node$condition, ...)
      call("while", condition, body)
    }

  list(exp = exp, succ = node$exit)
}
