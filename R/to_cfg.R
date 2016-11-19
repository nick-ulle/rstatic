#
# Methods for converting ASTNode objects to control-flow graphs.
#

#' @include basic_block.R
NULL

to_cfg = function(expr) {
}


#' @export
to_cfg_ = function(node, cfg = CFGraph$new()) {
  # If exit block is terminated, do nothing until change of branch.
  if (cfg$loop_open)
    UseMethod("to_cfg_")

  return (cfg)
}

#' @export
to_cfg_.If = function(node, cfg = CFGraph$new()) {
  entry_t = cfg$new_block()
  entry_f = cfg$new_block()
  cfg$branch(entry_t, entry_f, node$predicate)

  exit_t = to_cfg_(node$true, cfg)$exit

  if (is.null(node$false)) {
    exit = entry_f

  } else {
    # Switch branches.
    cfg$change_branch(entry_f)
    exit_f = to_cfg_(node$false, cfg)$exit

    exit = cfg$new_block()

    cfg$jump(from = exit_f, exit)
  }

  cfg$jump(from = exit_t, exit)

  return (cfg)
}


#' @export
to_cfg_.While = function(node, cfg = CFGraph$new()) {
  entry = cfg$new_block()
  cfg$jump(entry)

  entry_b = cfg$new_block()
  exit = cfg$new_block()
  cfg$branch(entry_b, exit, node$predicate)

  # Compute flow graph for loop body.
  cfg$loop_push(entry, exit)

  exit_b = to_cfg_(node$body, cfg)$exit
  if (cfg$loop_open)
    # Add the backedge.
    cfg$jump(from = exit_b, entry)

  cfg$loop_pop()

  # Switch branches.
  cfg$change_branch(exit)

  return (cfg)
}


#' @export
to_cfg_.For = function(node, cfg = CFGraph$new()) {
  entry = cfg$new_block()
  cfg$jump(entry)

  entry_b = cfg$new_block()
  exit = cfg$new_block()
  cfg$iterator(entry_b, exit, node$ivar, node$iter)

  # Compute flow graph for loop body.
  cfg$loop_push(entry, exit)

  exit_b = to_cfg_(node$body, cfg)$exit
  if (cfg$loop_open)
    # Add the backedge.
    cfg$jump(from = exit_b, entry)

  cfg$loop_pop()

  # Switch branches.
  cfg$change_branch(exit)

  return (cfg)
}


#' @export
to_cfg_.Break = function(node, cfg = CFGraph$new()) {
  cfg$loop_break()
  return (cfg)
}


#' @export
to_cfg_.Next = function(node, cfg = CFGraph$new()) {
  cfg$loop_next()
  return (cfg)
}


#' @export
to_cfg_.Call = function(node, cfg = CFGraph$new()) {
  cfg$exit_block$append(node)
  return (cfg)
}

#' @export
to_cfg_.Symbol = to_cfg_.Call
#' @export
to_cfg_.Assign = to_cfg_.Call
#' @export
to_cfg_.Literal = to_cfg_.Call

#' @export
to_cfg_.Bracket = function(node, cfg = CFGraph$new()) {
  # Handle all subexpressions; they'll automatically be added to the graph.
  lapply(node$body, to_cfg_, cfg)
  return (cfg)
}
