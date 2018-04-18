
to_data_frame = function(cfg) {
  # Create a data frame of lines from the control flow graph.
  lines = lapply(cfg$blocks, function(block) {
    lines = block$body

    len = length(lines)
    if (
      !is(lines[[len]], "ControlFlow") && !is(lines[[len]], "Return") &&
      block$id != "%exit"
    ) {
      browser()
    }

    ids = rep(block$id, length(lines))
    list(ids, lines)
  })
  
  block_ids = unlist(lapply(lines, `[[`, 1))
  lines = unlist(lapply(lines, `[[`, 2), recursive = FALSE, use.names = FALSE)

  class(lines) = "CodeList"

  code = data.frame(block = block_ids, line = I(lines),
    stringsAsFactors = FALSE)
  rownames(code) = NULL

  code
}


insert_after = function(code, i, line) {
  idx = seq(i, nrow(code))
  code[idx + 1, ] = code[idx, ]

  #code$line[i] = list(line) # Works
  #code[i, "line"] = list(list(line)) # Works
  code[[i, "line"]] = line

  code
}


extract_cfg = function(code, ...) {
  tails = block_tails(code$block)
  jumps = Map(get_jump, code[tails, "line"], code[tails, "block"])
  jumps = do.call(rbind, jumps)
  igraph::graph_from_edgelist(jumps)
}


get_jump = function(node, block) {
  UseMethod("get_jump")
}

get_jump.If = function(node, block = NULL) {
  matrix(c(block, block, node$true, node$false), nrow = 2)
}

get_jump.Loop = function(node, block = NULL) {
  matrix(c(block, block, node$body, node$exit), nrow = 2)
}

get_jump.Return = function(node, block = NULL) {
  matrix(c(block, "%exit"), nrow = 1)
}

get_jump.Symbol = function(node, block = NULL) {
  matrix(character(0), ncol = length(block) + 1)
}


toString.ASTNode = function(x, ...) {
  deparse_to_string(to_r(x, ...))
}

`[.CodeList` = function(x, i, ...) {
  if (length(i) == 1)
    .subset2(x, i, ...)
  else
    .subset(x, i, ...)
}


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

make_analysis = function(analysis) {
  function(pc, code, result, ...) {
    analysis(code[[pc, "line"]], result[[pc]], ...)
  }
}

forward_traversal =
function(code, analysis, propagate = union, cfg = extract_cfg(code), ...) {
  # FIXME: Parameters

  # Compute block locations.
  heads = block_heads(code$block)
  tails = block_tails(code$block)
  names(heads) = code[heads, "block"]
  names(tails) = code[tails, "block"]

  n = nrow(code)
  worklist = seq(1, n)
  result = replicate(n, character(0), simplify = FALSE)

  while (length(worklist) != 0) {
    pc = worklist[[1]]

    #while (pc != n+1) { # never n+1, since n is a tail with no successors
    repeat {
      worklist = setdiff(worklist, pc)

      new = analysis(pc, code, result, ...)

      # Get next line(s).
      if (pc %in% tails) {
        children = successors(code[pc, "block"], list(graph = cfg))
        children = heads[children]
      } else {
        children = pc + 1
      }

      # Test which lines need to be revisited.
      is_changed = !vapply(children, function(x) {
        # FIXME: check for subset unless we really need equality
        isTRUE(all.equal(new, result[[x]]))
      }, NA)
      to_visit = children[is_changed]

      if (length(to_visit) == 0)
        break

      result[to_visit] = lapply(result[to_visit], propagate, new)

      worklist = c(worklist, to_visit[-1])
      pc = to_visit[[1]]
    } # end while (pc != n)
  }

  result
}

backward_traversal = function(code, analysis, cfg = extract_cfg(code)) {
  # FIXME: Parameters

  # Compute block locations.
  heads = block_heads(code$block)
  tails = block_tails(code$block)
  names(heads) = code[heads, "block"]
  names(tails) = code[tails, "block"]

  n = nrow(code)
  worklist = seq(n, 1)
  #result = vector("list", n)
  result = replicate(n, character(0), simplify = FALSE)

  while (length(worklist) != 0) {
    pc = worklist[[1]]

    while (pc != 0) {
      worklist = setdiff(worklist, pc)
      line = code[pc, "line"]

      # TODO: new = analysis(line, result)
      new = analysis(line, result[[pc]])

      # Get next line(s).
      if (pc %in% heads) {
        children = predecessors(code[pc, "block"], list(graph = cfg))
        children = tails[children]
      } else {
        children = pc - 1
      }

      # Test which lines need to be revisited.
      is_changed = !vapply(children, function(x) {
        isTRUE(all.equal(new, result[[x]]))
      }, NA)
      to_visit = children[is_changed]

      if (length(to_visit) == 0)
        break

      result[to_visit] = lapply(result[to_visit], union, new)

      worklist = c(worklist, to_visit[-1])
      pc = to_visit[[1]]
    } # end while (pc != 0)
  }

  result
}


live_vars = function(node, result) {
  UseMethod("live_vars")
}

live_vars.Assign = function(node, result) {
  result = setdiff(result, node$write$name)

  live_vars(node$read, result)
}

live_vars.Symbol = function(node, result) {
  union(result, node$name)
}

live_vars.Application = function(node, result) {
  for (arg in node$args)
    result = live_vars(arg, result)

  result
}

live_vars.If = function(node, result) {
  live_vars(node$condition, result)
}

live_vars.Literal = function(node, result) result
