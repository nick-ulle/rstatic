#' Convert Blocks to Static Single-Assignment Form
#'
#' This function converts code blocks to static single-assignment form.
#'
#' This function modifies the blocks in place.
#'
#' @param node A Function or BlocksList object.
#' @param cfg (igraph) The control flow graph for the code.
#' @param dom_t (igraph) The dominator tree for the code.
#' @param in_place (logical) Modify the node in-place? If \code{FALSE}, the
#' node is copied.
#' @param ... Additional arguments to methods.
#' 
#' @return NULL, because this function modifies the blocks in place.
#'
#' @export
to_ssa =
function(node
  , cfg = compute_cfg(node)
  , dom_t = dominator_tree(cfg, 2)
  , in_place = FALSE
  , ...
  )
{ 
  dom_f = dominance_frontiers(cfg, dom_t)
  # For each block, get blocks for which the block is a dominance frontier.
  dominators = igraph::adjacent_vertices(dom_f, V(dom_f), "in")

  # Get definitions and live variables for each block.
  du = def_use_sets(node, by_block = TRUE, only_undefined_uses = TRUE)
  live = live_variables(node, cfg, du, full_analysis = TRUE)[["entry"]]

  if (!in_place)
    node = copy(node)

  ssa_insert_phis(node, du[["def"]], dominators, live)

  ssa_set_numbers(node, cfg, dom_t, ...)

  node
}


#' Insert SSA Phi-Functions
#'
#' This function inserts SSA phi-functions into Blocks in code.
#'
#' Generally, this function should only be called from \code{to_ssa()}.
#'
ssa_insert_phis =
function(node, defs_by_block, dominators, live)
{
  if (is(node, "Function"))
    node = node$body

  changed = TRUE
  while(changed) {
    changed = FALSE

    # Loop over blocks that are dominance frontiers, because these are where
    # phi-functions might be needed.
    for (b in seq_along(dominators)) {
      defs = defs_by_block[ dominators[[b]] ]
      defs = unlist(defs, recursive = FALSE, use.names = FALSE)
      # Remove defs that aren't live on entry to block b.
      defs = intersect(defs, live[[b]])

      inserted = node[[b]]$insert_phi(defs)
      if (any(inserted)) {
        defs_by_block[[b]] = c(defs_by_block[[b]], defs[inserted])
        changed = TRUE
      }
    } # end for
  }

  invisible(NULL)
}


#' Set SSA Numbers on Symbols
#'
#' This function sets the SSA numbers on Symbols in code.
#'
#' Generally, this function should only be called from \code{to_ssa()}.
#'
#' @param node (Function) The code object to number.
#' @param cfg (igraph) The control flow graph.
#' @param dom_t (igraph) The dominator tree for the CFG.
#' @param blocks (list) The blocks in the control flow graph.
#' @param active (integer) A vector of active SSA numbers, set by previous
#' blocks.
#' @param counter (Counter) A counter to generate unique SSA numbers.
#' @param ... Additional arguments to methods.
#'
ssa_set_numbers =
function(node, cfg, dom_t, blocks, active, counter, ...)
{
  UseMethod("ssa_set_numbers")
}

ssa_set_numbers.Function =
function(node
  , cfg = compute_cfg(node)
  , dom_t = dominator_tree(cfg, 2)
  , blocks = node$body$contents
  , active = integer(0)
  , counter = Counter$new()
  , ...)
{
  # FIXME: Set SSA numbers for default arguments. To do this, we need to
  # determine if and when default arguments are evaluated.
  for (p in node$params$contents)
    active = ssa_set_numbers_line(p, active, counter, ...)

  ssa_set_numbers.BlockList(node$body, cfg, dom_t, blocks, active, counter, ...)
}

ssa_set_numbers.BlockList =
function(node
  , cfg = compute_cfg(node)
  , dom_t = dominator_tree(cfg, 2)
  , blocks = node$contents
  , active = integer(0)
  , counter = Counter$new()
  , ...)
{
  ssa_set_numbers.Block(blocks[[2]], cfg, dom_t, blocks, active, counter, ...)
}

ssa_set_numbers.Block =
function(node, cfg, dom_t, blocks
  , active = integer(0)
  , counter = Counter$new()
  , ...)
{
  # Number lines in this block.
  for (line in c(node$phi, node$contents))
    active = ssa_set_numbers_line(line, active, counter, ...)

  # Number uses in phi-functions of CFG children.
  for (block in blocks[successors(cfg, node$id)]) {
    for (phi in block$phi)
      ssa_set_numbers_successor_phi(phi, active, node$id)
  }

  # Recurse to number dominator tree children.
  for (block in blocks[successors(dom_t, node$id)])
    ssa_set_numbers.Block(block, cfg, dom_t, blocks, active, counter, ...)

  active
}


ssa_set_numbers_line =
function(node, active, counter, ...)
{
  UseMethod("ssa_set_numbers_line")
}


# Definitions --------------------

ssa_increment =
function(node, active, counter, ...)
{
  basename = node$value
  n = counter$increment(basename)

  active[[basename]] = n
  node$ssa_number = n

  active
}


#' @export
ssa_set_numbers_line.Assign =
function(node, active, counter, ...)
{
  active = ssa_set_numbers_line(node$read, active, counter, ...)

  ssa_increment(node$write, active, counter)
}

#' @export
ssa_set_numbers_line.Phi =
function(node, active, counter, ...)
{
  ssa_increment(node$write, active, counter)
}

#' @export
ssa_set_numbers_line.Return = ssa_set_numbers_line.Assign

#' @export
ssa_set_numbers_line.For =
function(node, active, counter, ...)
{
  active = ssa_set_numbers_line(node$iterator, active, counter, ...)

  ssa_increment(node$variable, active, counter)
}

#' @export
ssa_set_numbers_line.Parameter =
function(node, active, counter, ...)
{
  ssa_increment(node, active, counter)
}


# Uses --------------------
ssa_set_numbers_successor_phi =
function(node, active, id)
{
  basename = node$write$value
  n = active[basename]
  symbol = Symbol$new(basename, n)

  node$set(id, symbol)

  active
}

#' @export
ssa_set_numbers_line.Symbol =
function(node, active, counter, ...)
{
  node$ssa_number = active[node$value]
  active
}


# Other Expressions --------------------

#' @export
ssa_set_numbers_line.ConditionalBranch =
function(node, active, counter, ...)
{
  ssa_set_numbers_line(node$condition, active, counter, ...)
}

#' @export
ssa_set_numbers_line.Invocation =
function(node, active, counter, ...)
{
  # NOTE: This assumes there are no assignments in the arguments.
  ssa_set_numbers_line(node$args, active, counter, ...)
}

#' @export
ssa_set_numbers_line.ArgumentList =
function(node, active, counter, ...)
{
  lapply(node$contents, ssa_set_numbers_line, active, counter, ...)
  active
}

#' @export
ssa_set_numbers_line.Call =
function(node, active, counter, ...)
{
  NextMethod()
  ssa_set_numbers_line(node$fn, active, counter, ...)
}

#' @export
ssa_set_numbers_line.Literal =
function(node, active, counter, ...)
{
  active
}

#' @export
ssa_set_numbers_line.Branch = ssa_set_numbers_line.Literal

#' @export
ssa_set_numbers_line.Function =
function(node, active, counter, ..., recursive = TRUE)
{
  if (recursive)
    to_ssa(node)
  active
}
