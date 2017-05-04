#
# Methods for converting ASTNode objects to control-flow graphs.
#

#' Build Control Flow Graph from R Expression
#'
#' This function builds the control flow graph (CFG) for an unquoted R
#' expression.
#'
#' @param expr An unquoted R expression.
#' @param ... Additional arguments to \code{to_cfg()}.
#'
#' @export
to_cfgq = function(expr, ...) {
  ast = to_ast(substitute(expr))
  to_cfg(ast, ...)
}


#' Build Control Flow Graph from ASTNodes
#'
#' This function builds the control flow graph (CFG) for a quoted R expression
#' or abstract syntax tree. When the root of the AST is a Function object, the
#' CFG is built for its body.
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
#' @param ast A quoted R expression or an abstract syntax tree.
#' @param in_place (logical) Don't copy AST before generating CFG?
#' @param ssa (logical) Return CFG in SSA form?
#'
#' @return The control flow graph as a CFGraph object. The \code{[[} operator
#' can be used to extract individual basic blocks from the graph.
#'
#' @export
to_cfg = function(ast, in_place = FALSE, ssa = TRUE) {
  UseMethod("to_cfg")
}

#' @export
to_cfg.Function = function(ast, in_place = FALSE, ssa = TRUE) {
  if (!in_place)
    ast = ast$copy()

  # Set up CFG for a function.
  cfg = ControlFlowGraph$new()
  cfg$set_params(ast$params)

  builder = CFGBuilder$new(cfg)

  build_cfg(ast$body, builder)

  # Always flow to the exit block.
  if (is.na(builder$insert_block))
    builder$insert_block = cfg$exit
  else if (builder$insert_block != cfg$exit)
    builder$create_br(cfg$exit)

  if (ssa)
    cfg = to_ssa(cfg, in_place = TRUE)

  return (cfg)
}

#' @export
to_cfg.ASTNode = function(ast, in_place = FALSE, ssa = TRUE) {
  if (!in_place)
    ast = ast$copy()

  cfg = ControlFlowGraph$new()
  builder = CFGBuilder$new(cfg)
  build_cfg(ast, builder)

  if (builder$insert_block != cfg$exit)
    builder$create_br(cfg$exit)

  if (ssa)
    cfg = to_ssa(cfg, in_place = TRUE)

  return (cfg)
}

#' @export
to_cfg.default = function(ast, in_place = FALSE, ssa = TRUE) {
  ast = to_ast(ast)
  to_cfg(ast, in_place = TRUE, ssa = ssa)
}


#' Build Basic Blocks from ASTNodes
#'
#' This helper function does a depth-first traversal of an AST in order to
#' build basic blocks for a CFG.
#'
#' Generally, this function should only be called from \code{to_cfg()}.
#'
#' @param node (ASTNode) An ASTNode to build the graph from.
#' @param builder (CFGBuilder) The graph builder.
#'
build_cfg = function(node, builder) {
  # Don't do anything if no insert block is set.
  if (is.na(builder$insert_block))
    invisible (NULL)
  
  UseMethod("build_cfg")
}

#' @export
build_cfg.If = function(node, builder) {
  entry_t = builder$new_block()
  entry_f = builder$new_block()
  builder$create_cond_br(entry_t, entry_f, node$condition)

  # FIXME: 
  exit = builder$new_block()

  build_cfg(node$true, builder)
  # Flow to the exit if control didn't flow elsewhere.
  if (!is.na(builder$insert_block))
    builder$create_br(exit)

  builder$insert_block = entry_f
  if (!is.null(node$false))
    build_cfg(node$false, builder)
  if (!is.na(builder$insert_block))
    builder$create_br(exit)

  builder$insert_block = exit
  invisible (NULL)
}


#' @export
build_cfg.While = function(node, builder) {
  entry = builder$new_block()
  builder$create_br(entry)

  entry_b = builder$new_block()
  exit = builder$new_block()
  builder$create_cond_br(entry_b, exit, node$condition)

  # Push a new context so break/next flow to the correct place.
  builder$loop_push(entry, exit)

  build_cfg(node$body, builder)
  if (!is.na(builder$insert_block))
    builder$create_br(entry)

  builder$loop_pop()

  builder$insert_block = exit
  invisible (NULL)
}


#' @export
build_cfg.For = function(node, builder) {
  # Loop Setup (before entry)
  # =========================
  # Initialize ._iter_ and ivar.
  iter_name = paste0("._iter_", node$ivar$base)
  def_iter = Assign$new(Symbol$new(iter_name), Integer$new(1L))
  def_i = Assign$new(
    write = Symbol$new(node$ivar$base),
    read  = Call$new("[[", list(node$iter, Symbol$new(iter_name)))
  )
  # FIXME:
  builder$append(def_iter)
  builder$append(def_i)

  # Loop Entry
  # ==========
  entry = builder$new_block()
  builder$create_br(entry)

  # Advance ._iter_ and ivar.
  adv_iter = Assign$new(
    write = Symbol$new(iter_name),
    read  = Call$new("+", list(Symbol$new(iter_name), Integer$new(1L)))
  )
  adv_i = Assign$new(
    write = Symbol$new(node$ivar$name),
    read  = Call$new("[[", list(node$iter$copy(), Symbol$new(iter_name)))
  )
  builder$cfg[[entry]]$append(adv_iter)
  builder$cfg[[entry]]$append(adv_i)

  # Loop Body
  # =========
  entry_b = builder$new_block()
  exit = builder$new_block()
  builder$create_iter(entry_b, exit, node$ivar, node$iter)

  # Push a new context so break/next flow to the correct place.
  builder$loop_push(entry, exit)

  build_cfg(node$body, builder)
  if (!is.na(builder$insert_block))
    builder$create_br(entry)

  builder$loop_pop()

  builder$insert_block = exit
  invisible (NULL)
}


#' @export
build_cfg.Break = function(node, builder) {
  builder$create_break()
  invisible (NULL)
}


#' @export
build_cfg.Next = function(node, builder) {
  builder$create_next()
  invisible (NULL)
}


#' @export
build_cfg.Return = function(node, builder) {
  # NOTE: We could keep the Return instead of creating a ._retval_ variable.

  assign = Assign$new(Symbol$new("._return_"), node$args[[1]])
  build_cfg(assign, builder)
  builder$create_ret()

  invisible (NULL)
}


#' @export
build_cfg.Brace = function(node, builder) {
  # Handle all subexpressions; they'll automatically be added to the graph.
  lapply(node$body, build_cfg, builder)
  invisible (NULL)
}


#' @export
build_cfg.Call = function(node, builder) {
  builder$append(node)
  invisible (NULL)
}

#' @export
build_cfg.Assign = build_cfg.Call
#' @export
build_cfg.Symbol = build_cfg.Call
#' @export
build_cfg.Literal = build_cfg.Call
# Bare function definitions do not change control flow.
#' @export
build_cfg.Function = build_cfg.Call


