#
# Methods for converting ASTNode objects to control-flow graphs.
#

#' @include basic_block.R
#' @include cfgraph.R
NULL

#' Generate Control Flow Graph for a Function
#'
#' This function generates the control flow graph for a Function object.
#'
#' @param func (Function) a Function to generate the graph for
#'
#' @export
to_cfg = function(fn) {
  if (!inherits(fn, "function"))
    stop("argument fn must be a function. Use to_cfg_ for arbitrary code.")

  cfg = CFGraph$new()
  cfg$exit_fn = cfg$new_block()

  to_cfg_(fn$body, cfg)

  # Connect exit block to exit_fn block.
  if (cfg$exit != cfg$exit_fn)
    cfg$jump(cfg$exit_fn)

  return (cfg)
}


#' @export
to_cfg_ = function(node, cfg = CFGraph$new()) {
  # If exit block is terminated, do nothing until change of branch.
  if (cfg$branch_open)
    UseMethod("to_cfg_")

  return (cfg)
}

#' @export
to_cfg_.If = function(node, cfg = CFGraph$new()) {
  entry_t = cfg$new_block()
  entry_f = cfg$new_block()
  cfg$branch(entry_t, entry_f, node$predicate)
  exit = cfg$new_block()

  exit_t = to_cfg_(node$true, cfg)$exit

  if (cfg$branch_open)
    cfg$jump(from = exit_t, exit)

  # Change to "false" branch.
  cfg$change_branch(entry_f)

  if (is.null(node$false))
    exit_f = entry_f
  else
    exit_f = to_cfg_(node$false, cfg)$exit

  if (cfg$branch_open)
    cfg$jump(from = exit_f, exit)

  cfg$exit = exit

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
  if (cfg$branch_open)
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
  cfg$iterate(entry_b, exit, node$ivar, node$iter)

  # Compute flow graph for loop body.
  cfg$loop_push(entry, exit)

  exit_b = to_cfg_(node$body, cfg)$exit
  if (cfg$branch_open)
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
to_cfg_.Return = function(node, cfg = CFGraph$new()) {
  assign = Assign$new(NULL)
  assign$read = node$args[[1]]
  assign$write = Symbol$new(assign, "__retval__")
  to_cfg_(assign, cfg)

  # NOTE: We could keep the Return instead of creating a __retval__ variable.
  #cfg$exit_block$append(node)

  cfg$fn_return()

  return (cfg)
}

#' @export
to_cfg_.Brace = function(node, cfg = CFGraph$new()) {
  # Handle all subexpressions; they'll automatically be added to the graph.
  lapply(node$body, to_cfg_, cfg)
  return (cfg)
}

#' @export
to_cfg_.Call = function(node, cfg = CFGraph$new()) {
  cfg$exit_block$append(node)
  return (cfg)
}

#' @export
to_cfg_.Assign = to_cfg_.Call

#' @export
to_cfg_.Symbol = to_cfg_.Call

#' @export
to_cfg_.Literal = to_cfg_.Call

# Bare function definitions do not change control flow.
#' @export
to_cfg_.Function = to_cfg_.Call


