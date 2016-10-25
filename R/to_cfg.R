#
# Methods for converting ASTNode objects to control-flow graphs.
#

#' @include basic_block.R
NULL

to_cfg = function(expr) {
}


to_cfg_ = function(node, block = BasicBlock$new(), state = CFGState$new()) {
  UseMethod("to_cfg_")
}


to_cfg_.If = function(node, block = BasicBlock$new(), state = CFGState$new()) {
  blk_true = BasicBlock$new()
  blk_false = BasicBlock$new()

  block$set_branch(node$predicate, blk_true, blk_false)

  # Compute flow graph for "true" branch.
  blk_true = to_cfg_(node$body, blk_true, state)

  # If "false" branch doesn't exist, use its block as the join block.
  if (is.null(node$false)) {
    blk_join = blk_false
  } else {
    blk_join = BasicBlock$new()

    # Compute flow graph for "false" branch.
    blk_false = to_cfg_(node$false, blk_false, state)
    blk_false$set_jump(blk_join)
  }

  blk_true$set_jump(blk_join)

  return (blk_join)
}


to_cfg_.While = function(node, block = BasicBlock$new(), state = CFGState$new()) {
  blk_guard = BasicBlock$new()
  blk_body = BasicBlock$new()
  blk_join = BasicBlock$new()
  
  block$set_jump(blk_guard)
  blk_guard$set_branch(node$predicate, blk_body, blk_join)

  blk_body = to_cfg_(node$body, blk_body, state)
  blk_body$set_jump(blk_guard)
  
  # FIXME: pop stack of break and next blocks.
  while (length(state$breaks) > 0) {
    state$breaks$pop()
  }

  while (length(state$nexts) > 0) {
    state$nexts$pop()
  }

  return (blk_join)
}


to_cfg_.For = function(node, block = BasicBlock$new(), state = CFGState$new()) {
  blk_guard = BasicBlock$new()
  blk_body = BasicBlock$new()
  blk_join = BasicBlock$new()
  
  block$set_jump(blk_guard)
  blk_guard$set_iterator(node$ivar, node$iter, blk_body, blk_join)

  blk_body = to_cfg_(node$body, blk_body, state)
  blk_body$set_jump(blk_guard)

  # FIXME: pop stack of break and next blocks.

  return (blk_join)
}


to_cfg_.Break = function(node, block = BasicBlock$new(), state = CFGState$new()) {
  # Push block onto stack of break blocks.
}


to_cfg_.Next = function(node, block = BasicBlock$new(), state = CFGState$new()) {
  # Push block to stack of next blocks.
}


to_cfg_.Call = function(node, block = BasicBlock$new(), state = CFGState$new()) {
  cfg$append_node(node)

  return (block)
}
