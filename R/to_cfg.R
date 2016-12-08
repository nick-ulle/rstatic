#
# Methods for converting ASTNode objects to control-flow graphs.
#

#' @include basic_block.R
#' @include cfgraph.R
NULL


#' Build Control Flow Graph for an AST
#'
#' This function builds the control flow graph (CFG) for an abstract syntax
#' tree. When the root of the AST is a Function object, the CFG is built for
#' its body.
#'
#' A control flow graph is a directed graph that represents the flow of control
#' in a program. Each node or "basic block" contains a linear sequence of
#' program instructions. Every basic block ends with a terminator instruction,
#' which branches to one or more basic blocks. Edges in the graph indicate
#' these branches.
#'
#' An if-statement makes a downward diamond in the graph and a while- or
#' for-loop makes a cycle.
#'
#' @param ast (ASTNode) An abstract syntax tree.
#'
#' @return The control flow graph as a CFGraph object. The \code{[[} operator
#' can be used to extract individual basic blocks from the graph.
#'
#' @export
to_cfg = function(ast) {
  if (!inherits(ast, "Function"))
    return (.to_cfg(ast))

  # The ast argument is a Function, so set up an exit block.
  cfg = CFGraph$new()
  cfg$exit_fn = cfg$new_block()

  .to_cfg(ast$body, cfg)

  # Connect exit block to exit_fn block.
  if (cfg$exit != cfg$exit_fn)
    cfg$jump(cfg$exit_fn)

  return (cfg)
}


.to_cfg = function(node, cfg = CFGraph$new()) {
  # If exit block is terminated, do nothing until change of branch.
  if (cfg$branch_open)
    UseMethod(".to_cfg")

  return (cfg)
}


.to_cfg.If = function(node, cfg = CFGraph$new()) {
  entry_t = cfg$new_block()
  entry_f = cfg$new_block()
  cfg$branch(entry_t, entry_f, node$predicate)
  exit = cfg$new_block()

  exit_t = .to_cfg(node$true, cfg)$exit

  if (cfg$branch_open)
    cfg$jump(from = exit_t, exit)

  # Change to "false" branch.
  cfg$change_branch(entry_f)

  if (is.null(node$false))
    exit_f = entry_f
  else
    exit_f = .to_cfg(node$false, cfg)$exit

  if (cfg$branch_open)
    cfg$jump(from = exit_f, exit)

  cfg$exit = exit

  return (cfg)
}


.to_cfg.While = function(node, cfg = CFGraph$new()) {
  entry = cfg$new_block()
  cfg$jump(entry)

  entry_b = cfg$new_block()
  exit = cfg$new_block()
  cfg$branch(entry_b, exit, node$predicate)

  # Compute flow graph for loop body.
  cfg$loop_push(entry, exit)

  exit_b = .to_cfg(node$body, cfg)$exit
  if (cfg$branch_open)
    # Add the backedge.
    cfg$jump(from = exit_b, entry)

  cfg$loop_pop()

  # Switch branches.
  cfg$change_branch(exit)

  return (cfg)
}


.to_cfg.For = function(node, cfg = CFGraph$new()) {
  entry = cfg$new_block()
  cfg$jump(entry)

  entry_b = cfg$new_block()
  exit = cfg$new_block()
  cfg$iterate(entry_b, exit, node$ivar, node$iter)

  # Compute flow graph for loop body.
  cfg$loop_push(entry, exit)

  exit_b = .to_cfg(node$body, cfg)$exit
  if (cfg$branch_open)
    # Add the backedge.
    cfg$jump(from = exit_b, entry)

  cfg$loop_pop()

  # Switch branches.
  cfg$change_branch(exit)

  return (cfg)
}


.to_cfg.Break = function(node, cfg = CFGraph$new()) {
  cfg$loop_break()
  return (cfg)
}


.to_cfg.Next = function(node, cfg = CFGraph$new()) {
  cfg$loop_next()
  return (cfg)
}


.to_cfg.Return = function(node, cfg = CFGraph$new()) {
  assign = Assign$new(NULL)
  assign$read = node$args[[1]]
  assign$write = Symbol$new(assign, "__retval__")
  .to_cfg(assign, cfg)

  # NOTE: We could keep the Return instead of creating a __retval__ variable.
  #cfg$exit_block$append(node)

  cfg$fn_return()

  return (cfg)
}


.to_cfg.Brace = function(node, cfg = CFGraph$new()) {
  # Handle all subexpressions; they'll automatically be added to the graph.
  lapply(node$body, .to_cfg, cfg)
  return (cfg)
}


.to_cfg.Call = function(node, cfg = CFGraph$new()) {
  cfg$exit_block$append(node)
  return (cfg)
}

.to_cfg.Assign = .to_cfg.Call
.to_cfg.Symbol = .to_cfg.Call
.to_cfg.Literal = .to_cfg.Call
# Bare function definitions do not change control flow.
.to_cfg.Function = .to_cfg.Call


