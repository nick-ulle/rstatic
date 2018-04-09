
# TODO: Add parameter to drop return statements.
rebuild_r = function(node, cfg) {
  UseMethod("rebuild_r")
}

rebuild_r.ControlFlowGraph = function(node, cfg) {
  c(exp, ) := rebuild_r.Block(node[[node$entry]], node)

  as.call(append(as.symbol("{"), exp))
}

rebuild_r.Block = function(node, cfg) {
  # Rebuild all but last line.
  len = length(node$body)
  lines = lapply(node$body[-len], rebuild_r2)

  last = node$body[[len]]

  # Rebuild last line.
  succ = NA
  if (is(last, "If") || is(last, "For")) {
    c(line, succ) := rebuild_r(last, cfg)

  } else if (is(last, "Return")) {
    line = rebuild_r2.Return(last)

  } else {
    line = rebuild_r2(last)
    succ = successors(node$id, cfg)
  }

  lines = append(lines, line)
  if (is.na(succ))
    return (list(exp = lines, succ = succ))

  # Rebuild blocks until there's a depth change.
  next_block = cfg[[succ]]
  if (node$depth != next_block$depth) 
    return (list(exp = lines, succ = succ))

  c(next_lines, succ) := rebuild_r.Block(next_block, cfg)
  lines = append(lines, next_lines)

  list(exp = lines, succ = succ)
}

rebuild_r.For = function(node, cfg) {
  # Rebuild the body.
  body = node$body
  c(lines, ) := rebuild_r.Block(cfg[[body]], cfg)
  lines = append(as.symbol("{"), lines)
  body = as.call(lines)

  variable = rebuild_r2.Symbol(node$ivar)
  iterator = rebuild_r2(node$iter)

  exp = call("for", variable, iterator, body)

  list(exp = exp, succ = node$exit)
}

rebuild_r.While = function(node, cfg) {
  body = node$body
  c(lines, ) := rebuild_r.Block(cfg[[body]], cfg)
  lines = append(as.symbol("{"), lines)
  body = as.call(lines)

  condition = rebuild_r2(node$condition)

  exp = call("while", condition, body)

  list(exp = exp, succ = node$exit)
}

rebuild_r.If = function(node, cfg) {
  # Rebuild the true branch.
  true = node$true
  c(lines, succ) := rebuild_r.Block(cfg[[true]], cfg)

  lines = append(as.symbol("{"), lines)
  true = as.call(lines)

  # Rebuild the false branch.
  false = node$false
  c(lines, succ2) := rebuild_r.Block(cfg[[false]], cfg)

  lines = append(as.symbol("{"), lines)
  false = as.call(lines)
  
  # Rebuild the condition.
  condition = rebuild_r2(node$condition)

  # Assemble into an if-statement.
  exp = call("if", condition, true, false)

  # Return successor that's not from a return/break/next
  if (is.na(succ))
    succ = succ2
  else if (is.na(succ2))
    succ = succ
  else if (succ != succ2)
    stop("if-statement has successor conflict.")

  list(exp = exp, succ = succ)
}


# ----------------------------------------

rebuild_r2 = function(node)
  UseMethod("rebuild_r2")

rebuild_r2.Brace = function(node) {
  browser()
}

rebuild_r2.Assign = function(node) {
  read = rebuild_r2(node$read)
  write = rebuild_r2(node$write)
  call("=", write, read)
}

rebuild_r2.Return = function(node) {
  call("return", rebuild_r2(node$read))
}

rebuild_r2.Call = function(node) {
  fn = to_r(node$fn)
  args = lapply(node$args, to_r)
  as.call(append(fn, args))
}

rebuild_r2.Symbol = function(node) as.symbol(node$name)
rebuild_r2.Literal = function(node) node$value
