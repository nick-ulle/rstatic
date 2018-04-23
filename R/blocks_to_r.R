# FIXME: blocks_to_r() probably doesn't generate code for nested functions.

#' Generate R Code from Basic Blocks
#'
#' This function converts basic blocks back to R code.
#'
#' @param node The basic blocks to convert.
#' @param ... Optional arguments to and from methods.
#'
#' @export
blocks_to_r = function(node, ...) {
  UseMethod("blocks_to_r")
}

#' @export
blocks_to_r.data.frame =
function(node, ...) {
  blocks_to_r.BlocksList(as_blocks(node, ...), ...)
}

#' @export
blocks_to_r.BlocksList =
function(node, ...) {
  c(exp, ) := blocks_to_r.Block(node[[1]], blocks = node, ...)

  as.call(append(as.symbol("{"), exp))
}



#' @export
blocks_to_r.Branch = function(node, blocks, ...) {
  list(to_r.Branch(node, ...), node$target$name)
}

#' @export
blocks_to_r.Break = function(node, blocks, ...) {
  list(to_r(node, ...), NA)
}

#' @export
blocks_to_r.Next = blocks_to_r.Break

#' @export
blocks_to_r.Return = blocks_to_r.Break

#' @export
blocks_to_r.Block = function(node, blocks, ...) {
  all_lines = list()
  # Append blocks until there's a depth change.
  repeat {
    len = length(node)

    lines = lapply(node[-len], to_r, ...)
    c(last, succ) := blocks_to_r(node[[len]], blocks, ...)
    all_lines = c(all_lines, lines, last)

    if (is.na(succ))
      break

    next_block = blocks[[succ]]
    if (node$depth != next_block$depth)
      break

    node = next_block
  }

  list(exp = all_lines, succ = succ)
}


# Helper function to generate a braced expression from a Block.
blocks_to_r_brace = function(id, blocks, ...) {
  gen = blocks_to_r.Block(blocks[[id]], blocks, ...)
  lines = append(as.symbol("{"), gen[[1]])
  gen[[1]] = as.call(lines)

  gen
}


#' @export
blocks_to_r.If = function(node, blocks, ...) {
  # Rebuild the true branch.
  c(true, succ_t)  := blocks_to_r_brace(node$true$name, blocks, ...)
  c(false, succ_f) := blocks_to_r_brace(node$false$name, blocks, ...)

  # Return successor that's not caused by return/break/next.
  succ = setdiff(c(succ_t, succ_f), NA)
  if (length(succ) != 1)
    stop("if-statement has successor conflict.")

  condition = to_r(node$condition, ...)

  # Assemble into an if-statement.
  exp = call("if", condition, true, false)

  list(exp = exp, succ = succ)
}


#' @export
blocks_to_r.For = function(node, blocks, ...) {
  c(body, ) := blocks_to_r_brace(node$body$name, blocks, ...)
  variable = to_r.Symbol(node$variable, ...)
  iterator = to_r(node$iterator, ...)

  exp = call("for", variable, iterator, body)

  list(exp = exp, succ = node$exit$name)
}


#' @export
blocks_to_r.While =
function(node, blocks, ...) {
  c(body, ) := blocks_to_r_brace(node$body$name, blocks, ...)

  exp =
    if (node$is_repeat) {
      call("repeat", body)
    } else {
      condition = to_r(node$condition, ...)
      call("while", condition, body)
    }

  list(exp = exp, succ = node$exit$name)
}
