#
# Methods for converting ASTNode objects to control-flow graphs.
#

#' @include basic_block.R
NULL

to_cfg = function(expr) {
}


to_cfg_ = function(node, builder = CFGBuilder$new()) {
  # If the current block is terminated, ascend to the call that created the
  # current block (that is, stop adding instructions).
  if (builder$block$is_terminated)
    return (NULL)

  UseMethod("to_cfg_")
}


to_cfg_.If = function(node, builder = CFGBuilder$new()) {
  # NOTE: a separate entry block is not strictly necessary.
  blk_entry = BasicBlock$new()
  builder$block$set_branch(blk_entry)

  blk_true = BasicBlock$new()
  blk_false = BasicBlock$new()
  blk_entry$set_branch(blk_true, blk_false, node$predicate)

  # Compute flow graph for "true" branch.
  builder$block = blk_true
  to_cfg_(node$true, builder)

  if (is.null(node$false)) {
    # If "false" branch doesn't exist, use its block as the exit block.
    blk_exit = blk_false
  } else {
    # Compute flow graph for "false" branch.
    builder$block = blk_false
    to_cfg_(node$false, builder)

    blk_exit = BasicBlock$new()
    builder$block$set_branch(blk_exit)
  }

  blk_true$set_branch(blk_exit)
  builder$block = blk_exit

  return (blk_exit)
}


to_cfg_.While = function(node, builder = CFGBuilder$new()) {
  blk_entry = BasicBlock$new()
  builder$block$set_branch(blk_entry)

  blk_body = BasicBlock$new()
  blk_exit = BasicBlock$new()
  blk_entry$set_branch(blk_body, blk_exit, node$predicate)

  # Compute flow graph for loop body.
  builder$loop_entry$push(blk_entry)
  builder$loop_exit$push(blk_exit)
  builder$block = blk_body

  to_cfg_(node$body, builder)

  builder$loop_entry$pop()
  builder$loop_exit$pop()

  # Add the backedge; breaks and nexts were already handled.
  builder$block$set_branch(blk_entry)

  return (blk_exit)
}


to_cfg_.For = function(node, builder = CFGBuilder$new()) {
  blk_entry = BasicBlock$new()
  builder$block$set_branch(blk_entry)

  blk_body = BasicBlock$new()
  blk_exit = BasicBlock$new()
  blk_entry$set_iterate(blk_body, blk_exit, node$ivar, node$iter)

  builder$loop_entry$push(blk_entry)
  builder$loop_exit$push(blk_exit)
  builder$block = blk_body

  to_cfg_(node$body, builder)

  builder$loop_entry$pop()
  builder$loop_exit$pop()

  builder$block$set_branch(blk_entry)

  return (blk_exit)
}


to_cfg_.Break = function(node, builder = CFGBuilder$new()) {
  builder$block$set_jump(builder$loop_exit$peek())
}


to_cfg_.Next = function(node, builder = CFGBuilder$new()) {
  builder$block$set_jump(builder$loop_entry$peek())
}


to_cfg_.Call = function(node, builder = CFGBuilder$new()) {
  builder$block$body = append(builder$block$body, node)

  return (builder$block)
}

to_cfg_.Bracket = function(node, builder = CFGBuilder$new()) {
  lapply(node$body, to_cfg_, builder)

  return (builder$block)
}
