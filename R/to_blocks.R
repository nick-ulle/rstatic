#
# Methods for converting ASTNode objects to control-flow graphs.
#

# Data Frames ----------------------------------------

#' @rdname as_data_frame
#'
#' @export
as.data.frame.FunctionBlocks =
function(x, ...) {
  x = x$blocks
  lens = vapply(x, length, 0L)
  ids = rep(seq_along(x), lens)

  depths = vapply(x, function(b) b$depth, 0L)
  depths = rep(depths, lens)

  lines = lapply(x, function(block) block$body)
  lines = unlist(lines, recursive = FALSE, use.names = FALSE)
  class(lines) = "CodeList"

  data.frame(line = I(lines), block = ids, depth = depths)
}

#' Convert Basic Blocks to a Code Data Frame
#'
#' This function converts basic blocks to a data frame where each row is one
#' ``line'' of code.
#'
#' @param x (FunctionBlocks) The basic blocks to convert.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @export
as_data_frame = function(x, ...) {
  UseMethod("as_data_frame")
}

#' @rdname as_data_frame
#'
#' @export
as_data_frame.FunctionBlocks = as.data.frame.FunctionBlocks


#' Convert Code Data Frame to Basic Blocks
#'
#' This function converts a code data frame to basic blocks.
#'
#' @param code (data.frame) The data frame to convert.
#' @param new_blocks (logical) Whether to create new Block objects. Beware that
#' these new blocks will become the new parents of the expressions in the
#' block.
#'
#' @export
as_blocks = function(code, new_blocks = FALSE) {
  heads = block_heads(code[["block"]])

  if (new_blocks) {
    blocks = split(code, code$block)
    blocks = lapply(blocks, function(b) {
      Block$new(unclass(b[["line"]]), b[[1, "id"]], b[[1, "depth"]])
    })
    idx = as.numeric(names(blocks))

  } else {
    blocks = lapply(heads, function(i) code[[i, "line"]]$parent)
    idx = code[heads, "block"]
  }

  result = list()
  result[idx] = blocks

  result
}


# Blocks ----------------------------------------

#' Convert an R Expression to Basic Blocks
#'
#' This function converts an unquoted R expression to basic blocks.
#'
#' @param expr An unquoted R expression.
#' @param ... Additional arguments to \code{to_blocks()}.
#'
#' @export
quote_blocks = function(expr, ...) {
  ast = to_ast(substitute(expr))
  to_blocks(ast, in_place = TRUE, ...)
}


#' Convert an ASTNode Object to Basic Blocks
#'
#' This function converts an ASTNode object or a quoted R expression into a
#' list of basic blocks. When the root of the AST is a Function object, the
#' CFG is built for its body.
#'
#' Each basic block contains a sequence of non-branching program instructions.
#' Each basic block ends with an instruction that branches to one or more other
#' basic blocks.
#'
#' @param node A quoted R expression or an abstract syntax tree.
#' @param in_place (logical) Operate on the node in place? If \code{TRUE}, the
#' node will be modified.
#' @param ssa (logical) Also convert to static single assignment form?
#' @param insert_return (logical) Apply \code{insert_return()} to the node
#' before generating the CFG?
#'
#' @return A Function node with the control flow graph in its \code{$cfg}
#' field.
#'
#' @export
to_blocks =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE)
{
  UseMethod("to_blocks")
}

#' @export
to_blocks.ASTNode =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE) {
  if (!in_place)
    node = node$copy()

  if (insert_return)
    node = insert_return(node)

  helper = c(
    this_block = NA, sib_block = -1, #"%exit", #cfg$exit,
    next_block = NA, break_block = NA)

  if (!is(node, "Brace"))
    node = Brace$new(node, is_hidden = TRUE)

  c(blocks, ) := create_block_list(node, helper)

  # Sort the blocks in reverse postorder to make them easier to read and ensure
  # SSA numbers will increase monotonically.
  #ordering = rev(postorder(cfg))
  #cfg$reorder(ordering)

  node = FunctionBlocks$new(list(), blocks, is_hidden = TRUE)

  if (ssa)
    to_ssa(node)

  node
}


#' @export
to_blocks.Function =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE) {
  params = node$params

  node = to_blocks.ASTNode(node$body, in_place, ssa = FALSE, insert_return)

  node$params = params
  node$is_hidden = FALSE

  # TODO: Optionally insert default argument evaluation points into generated
  # code.

  if (ssa)
    to_ssa(node)

  node
}

#' @export
to_blocks.default =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE) {
  node = to_ast(node)
  to_blocks(node, in_place = TRUE, ssa, insert_return)
}


# Options for places a block can go:
# 
# * `exit_block`: last block in Function (e.g., from a return). Never
#   changes.
#
# * `top_block`: parent's sibling (e.g., at the end of an if-statement). This
#   changes when a list of blocks occurs.
#
# * `sib_block`: header of current loop
#
# * `break_block`: `top_block` of current loop. Does this need to be separate
#   from `top_block`?


create_block_list = function(node, helper, cfg = list(), depth = 1L) {
  UseMethod("create_block_list")
}


create_block_list.Brace = function(node, helper, cfg = list(), depth = 1L) {
  # Split into blocks and add them to the graph.
  blocks = split_blocks(node)

  # Set block IDs.
  siblings = length(cfg) + seq_along(blocks)
  Map(function(b, id) b$id = id, blocks, siblings)

  cfg = c(cfg, blocks)

  # After last block, branch to parent's sibling block.
  entry = siblings[[1]] # Index of first new block 
  siblings = c(siblings[-1], helper[["sib_block"]])

  # Build the subgraph for each block.
  for (i in seq_along(siblings)) {
    helper[["sib_block"]] = siblings[[i]]
    c(cfg, ) := create_block_list.Block(blocks[[i]], helper, cfg, depth)
    if (i == 1)
      helper[["this_block"]] = NA
  }

  list(cfg, entry)
}


#' @export
create_block_list.Block =
function(node, helper, cfg = list(), depth = 1L) {
  helper[["this_block"]] = node$id
  node$depth = depth

  # Only the final expression affects control flow.
  len = length(node$body)
  if (len > 0)
    c(cfg, ) := create_block_list(node$body[[len]], helper, cfg, depth)
  else
    # An empty block is equivalent to a block that ends with non-control flow.
    c(cfg, ) := create_block_list.ASTNode(node, helper, cfg, depth)

  # Check for function definitions. This must be done here, after the block has
  # had labels inserted.
  defs = find_nodes(node, is, "Function")
  for (d in defs) {
    fn = to_blocks.Function(node[[d]], in_place = TRUE, ssa = FALSE,
      insert_return = FALSE)
    node[[d]] = fn
  }

  list(cfg, NA)
}

#' @export
create_block_list.If =
function(node, helper, cfg = list(), depth = 1L) {
  # Process true branch, then false branch.
  c(cfg, id_true) := create_block_list.Brace(node$true, helper, cfg, depth + 1L)
  node$true = Label$new(id_true)

  c(cfg, id_false) := create_block_list.Brace(node$false, helper, cfg, depth + 1L)
  node$false = Label$new(id_false)

  list(cfg, NA)
}

#' @export
create_block_list.Loop =
function(node, helper, cfg = list(), depth = 1L) {
  # break_block = sib_block  (the original sibling is the exit block)
  # next_block  = this_block (this block is the test block)
  # sib_block   = this_block (the last body block must loop back)
  helper[["break_block"]] = helper[["sib_block"]]
  helper[["next_block"]]  = helper[["this_block"]]
  helper[["sib_block"]]   = helper[["this_block"]]

  c(cfg, id_body) := create_block_list.Brace(node$body, helper, cfg, depth + 1L)
  node$body = Label$new(id_body)

  # Add edge to exit loop.
  node$exit = Label$new(helper[["break_block"]])

  list(cfg, NA)
}


# Edge-adding Cases ----------------------------------------
#' @export
create_block_list.ASTNode =
function(node, helper, cfg = list(), depth = 1L) {
  # In this case, control ascends from the current control structure.
  # TODO: Add a branch instruction?
  #cfg$add_edge(helper[["this_block"]], helper[["sib_block"]])

  # So we add a Branch instruction.
  br = Branch$new(Label$new(helper[["sib_block"]])) # TODO:

  this_block = helper[["this_block"]]
  cfg[[this_block]]$body = c(cfg[[this_block]]$body, br)

  list(cfg, NA)
}

#' @export
create_block_list.Break =
function(node, helper, cfg = list(), depth = 1L) {
  if (is.na(helper[["break_block"]])) {
    w = 'invalid use of Break. No outgoing edge will be added for block "%s".'
    warning(sprintf(w, helper[["this_block"]]))

  } else {
    node$target = Label$new(helper[["break_block"]])
  }

  list(cfg, NA)
}

#' @export
create_block_list.Next =
function(node, helper, cfg = list(), depth = 1L) {
  if (is.na(helper[["next_block"]])) {
    w = 'invalid use of Next. No outgoing edge will be added for block "%s".' 
    warning(sprintf(w, helper[["this_block"]]))

  } else {
    node$target = Label$new(helper[["next_block"]])
  }

  list(cfg, NA)
}

#' @export
create_block_list.Return =
function(node, helper, cfg = list(), depth = 1L) {
  # Link to exit block.
  node$target = Label$new("exit")

  list(cfg, NA)
}


# Helper function to split a Brace into Blocks.
split_blocks = function(node) {
  flows = vapply(node$body, function(line) {
    c(is(line, "ControlFlow"), is(line, "Loop"))
  }, logical(2))
  is_loop = flows[2, ]
  flows = flows[1, ]

  # Shift over by one element so each block starts after a flow.
  flows = c(FALSE, head(flows, -1))

  # Put each loop in its own block.
  flows[is_loop] = TRUE

  blocks = split(node$body, cumsum(flows))
  blocks = lapply(blocks, Block$new)

  names(blocks) = NULL
  blocks
}
