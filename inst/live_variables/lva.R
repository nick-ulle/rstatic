
library(rstatic)

toy = quote_cfg({
  x = 3
  y = x
}, ssa = FALSE)

# Establish baseline by computing the killed and used variables in one block.

toy2 = quote_cfg({
  x = 3
  y = f(x, z)
}, ssa = FALSE)

toy3 = quote_cfg({
  x = 3
  y = 1
  z = 1
  if (x > 1) {
    y = x
  } else {
    z = x
  }

  a = y + z
}, ssa = FALSE)


# Now that we can compute the KU sets, we need to iterate to find the live
# variables.

live_variables = function(fn, max_iter = 20) {
  ku = compute_ku(fn)

  # Initialize entry and exit set for each block.
  live = vector("list", length(fn$cfg))
  live[] = list( list(entry = character(0), exit = character(0)) )
  names(live) = names(fn$cfg$blocks)

  for (i in 1:max_iter) {
    old_live = live

    # Compute entry and exit set for each block.
    for (b in names(fn$cfg$blocks)) {
      live[[b]][["entry"]] =
        union(setdiff(live[[b]][["exit"]], ku[[b]]$killed), ku[[b]]$used)

      out = igraph::neighbors(fn$cfg$graph, b, "out")
      combine = character(0)
      for (x in names(out))
        combine = union(combine, live[[x]][["entry"]])
      live[[b]][["exit"]] = combine
    }

    if (identical(old_live, live))
      break
  }

  message(sprintf("Solution found after %i iterations.", i))
  live
}


# Compute KU for every block in a graph.
compute_ku = function(fn) {
  ku = list(killed = character(0), used = character(0))
  lapply(fn$cfg$blocks, compute_ku_ast, ku)
}


# Compute KU for a single block.
compute_ku_ast = function(node, ku) {
  UseMethod("compute_ku_ast")
}

compute_ku_ast.Brace = function(node, ku) {
  # Kills only happen in LHS of assignment. Also `rm()`
  # Uses can be widespread.
  for (line in node$body) {
    ku = compute_ku_ast(line, ku)
  }

  ku
}

compute_ku_ast.Assign = function(node, ku) {
  ku = compute_ku_ast(node$read, ku)

  # Add LHS to killed.
  ku$killed = union(ku$killed, node$write$name)

  ku
}

compute_ku_ast.If = function(node, ku) {
  compute_ku_ast(node$condition, ku)
}

compute_ku_ast.Symbol = function(node, ku) {
  used = node$name

  # Check whether this symbol has already been killed in this block.
  if (used %in% ku$killed)
    return (ku)

  ku$used = union(ku$used, used)
  ku
}

compute_ku_ast.Call = function(node, ku) {
  for (arg in node$args) {
    ku = compute_ku_ast(arg, ku)
  }

  ku
}

compute_ku_ast.Literal = function(node, ku) ku
