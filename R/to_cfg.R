#
# Methods for converting ASTNode objects to control-flow graphs.
#


#' Build Control Flow Graph from ASTNodes
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
#' @param in_place (logical) Don't copy AST before generating CFG?
#' @param as_ssa (logical) Return CFG in SSA form?
#'
#' @return The control flow graph as a CFGraph object. The \code{[[} operator
#' can be used to extract individual basic blocks from the graph.
#'
#' @export
to_cfg = function(ast, in_place = FALSE, as_ssa = TRUE) {
  UseMethod("to_cfg")
}

#' @export
to_cfg.Function = function(ast, in_place = FALSE, as_ssa = TRUE) {
  if (!in_place)
    ast = ast$copy()

  # Set up CFG for a function.
  cfg = CFGraph$new(kind = "function")
  cfg[[cfg$entry]]$set_params(ast$params)

  to_basic_blocks(ast$body, cfg)

  # Connect exit block to exit_fn block.
  if (cfg$exit != cfg$exit_fn)
    cfg$jump(cfg$exit_fn)

  if (as_ssa)
    cfg = to_ssa(cfg, in_place = TRUE)

  return (cfg)
}

#' @export
to_cfg.ASTNode = function(ast, in_place = FALSE, as_ssa = TRUE) {
  if (!in_place)
    ast = ast$copy()

  cfg = to_basic_blocks(ast)

  if (as_ssa)
    cfg = to_ssa(cfg, in_place = TRUE)

  return (cfg)
}

#' @export
to_cfg.default = function(ast, in_place = FALSE, as_ssa = TRUE) {
  msg = sprintf(
    "Cannot convert object of class '%s' to CFG.", class(ast)[[1]]
  )
  stop(msg)
}


#' Build Basic Blocks from ASTNodes
#'
#' This helper function does a depth-first traversal of an AST in order to
#' build basic blocks for a CFG.
#'
#' Generally, this function should only be called from \code{to_cfg()}.
#'
#' @param node (ASTNode) An ASTNode to build basic blocks from.
#' @param cfg (CFGraph) A CFG to insert the basic blocks into.
#'
to_basic_blocks = function(node, cfg = CFGraph$new()) {
  # If exit block is terminated, do nothing until change of branch.
  if (cfg$branch_open)
    UseMethod("to_basic_blocks")

  return (cfg)
}

#' @export
to_basic_blocks.If = function(node, cfg = CFGraph$new()) {
  entry_t = cfg$new_block()
  entry_f = cfg$new_block()
  cfg$branch(entry_t, entry_f, node$condition)
  exit = cfg$new_block()

  exit_t = to_basic_blocks(node$true, cfg)$exit

  if (cfg$branch_open)
    cfg$jump(from = exit_t, exit)

  # Change to "false" branch.
  cfg$change_branch(entry_f)

  if (is.null(node$false))
    exit_f = entry_f
  else
    exit_f = to_basic_blocks(node$false, cfg)$exit

  if (cfg$branch_open)
    cfg$jump(from = exit_f, exit)

  cfg$exit = exit

  return (cfg)
}


#' @export
to_basic_blocks.While = function(node, cfg = CFGraph$new()) {
  entry = cfg$new_block()
  cfg$jump(entry)

  entry_b = cfg$new_block()
  exit = cfg$new_block()
  cfg$branch(entry_b, exit, node$condition)

  # Compute flow graph for loop body.
  cfg$loop_push(entry, exit)

  exit_b = to_basic_blocks(node$body, cfg)$exit
  if (cfg$branch_open)
    # Add the backedge.
    cfg$jump(from = exit_b, entry)

  cfg$loop_pop()

  # Switch branches.
  cfg$change_branch(exit)

  return (cfg)
}


#' @export
to_basic_blocks.For = function(node, cfg = CFGraph$new()) {
  # Initialize ._iter_ in block before entry block.
  iter_name = paste0("._iter_", node$ivar$base)
  def_iter = Assign$new(Symbol$new(iter_name), Integer$new(1L))
  def_i = Assign$new(
    write = Symbol$new(node$ivar$base),
    read  = Call$new("[[", list(node$iter, Symbol$new(iter_name)))
  )
  cfg$exit_block$append(def_iter)
  cfg$exit_block$append(def_i)

  # Create entry block; advance ._iter_ and ivar here.
  entry = cfg$new_block()
  cfg$jump(entry)

  adv_iter = Assign$new(
    write = Symbol$new(iter_name),
    read  = Call$new("+", list(Symbol$new(iter_name), Integer$new(1L)))
  )
  adv_i = Assign$new(
    write = Symbol$new(node$ivar$name),
    read  = Call$new("[[", list(node$iter$copy(), Symbol$new(iter_name)))
  )
  cfg[[entry]]$append(adv_iter)
  cfg[[entry]]$append(adv_i)

  # Create exit block and first body block; check loop condition first in body.
  entry_b = cfg$new_block()
  exit = cfg$new_block()
  cfg$iterate(entry_b, exit, node$ivar, node$iter)

  # Compute flow graph for loop body.
  cfg$loop_push(entry, exit)

  exit_b = to_basic_blocks(node$body, cfg)$exit
  if (cfg$branch_open)
    # Add the backedge.
    cfg$jump(from = exit_b, entry)

  cfg$loop_pop()

  # Set CFG exit block reference to the exit block.
  cfg$change_branch(exit)

  return (cfg)
}


#' @export
to_basic_blocks.Break = function(node, cfg = CFGraph$new()) {
  cfg$loop_break()
  return (cfg)
}


#' @export
to_basic_blocks.Next = function(node, cfg = CFGraph$new()) {
  cfg$loop_next()
  return (cfg)
}


#' @export
to_basic_blocks.Return = function(node, cfg = CFGraph$new()) {
  assign = Assign$new(Symbol$new("._return_"), node$args[[1]])
  to_basic_blocks(assign, cfg)

  # NOTE: We could keep the Return instead of creating a ._retval_ variable.
  #cfg$exit_block$append(node)

  cfg$fn_return()

  return (cfg)
}


#' @export
to_basic_blocks.Brace = function(node, cfg = CFGraph$new()) {
  # Handle all subexpressions; they'll automatically be added to the graph.
  lapply(node$body, to_basic_blocks, cfg)
  return (cfg)
}


#' @export
to_basic_blocks.Call = function(node, cfg = CFGraph$new()) {
  cfg$exit_block$append(node)
  return (cfg)
}

#' @export
to_basic_blocks.Assign = to_basic_blocks.Call
#' @export
to_basic_blocks.Symbol = to_basic_blocks.Call
#' @export
to_basic_blocks.Literal = to_basic_blocks.Call
# Bare function definitions do not change control flow.
#' @export
to_basic_blocks.Function = to_basic_blocks.Call


