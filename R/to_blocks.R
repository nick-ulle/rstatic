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
to_blocks.Brace =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE) {
  if (!in_place)
    node = copy(node)

  if (insert_return)
    node = insert_return(node)

  # FIXME: Include a return_block so that return isn't hard-coded to 1.
  helper = c(
    this_block = NA, sib_block = 1L,
    next_block = NA, break_block = NA)

  blocks = list(Block$new(Symbol$new("._return_"), id = 1L, depth = 1L))
  c(blocks, ) := create_block_list(node, helper, blocks)

  # Sort the blocks in reverse postorder to make them easier to read and ensure
  # SSA numbers will increase monotonically.
  #ordering = rev(postorder(cfg))
  #cfg$reorder(ordering)

  node = BlockList$new(blocks)

  if (ssa)
    to_ssa(node, in_place = TRUE)

  node
}


#' @export
to_blocks.Function =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE) {
  if (!in_place)
    node = copy(node)

  node$body = to_blocks.Brace(node$body, in_place = FALSE, ssa = FALSE,
    insert_return)

  # TODO: Optionally insert default argument evaluation points into generated
  # code.

  if (ssa)
    to_ssa(node, in_place = TRUE)

  node
}

to_blocks.ASTNode =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE) {
  if (!in_place)
    node = copy(node)

  to_blocks.Brace(Brace$new(node), in_place = TRUE, ssa, insert_return)
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
  blocks = split_blocks(node$contents)

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
  len = length(node$contents)
  if (len > 0)
    c(cfg, ) := create_block_list(node$contents[[len]], helper, cfg, depth)
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
  cfg[[this_block]]$contents = c(cfg[[this_block]]$contents, br)

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
  node$target = Label$new(1)

  list(cfg, NA)
}


# Helper function to split a list of lines into Blocks.
split_blocks = function(lines) {
  flows = vapply(lines, function(line) {
    c(is(line, "ControlFlow"), is(line, "Loop"))
  }, logical(2))
  is_loop = flows[2, ]
  flows = flows[1, ]

  # Shift over by one element so each block starts after a flow.
  flows = c(FALSE, head(flows, -1))

  # Put each loop in its own block.
  flows[is_loop] = TRUE

  blocks = split(lines, cumsum(flows))
  blocks = lapply(blocks, Block$new)

  names(blocks) = NULL
  blocks
}
