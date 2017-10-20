#
# Methods for converting ASTNode objects to control-flow graphs.
#

#' Build Control Flow Graph from R Expression
#'
#' This function builds the control flow graph (CFG) for an unquoted R
#' expression.
#'
#' @param expr An unquoted R expression.
#' @param ... Additional arguments to \code{toCFG()}.
#'
#' @export
toCFGq = function(expr, ...) {
  ast = toAST(substitute(expr))
  toCFG(ast, ...)
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
#' @param inPlace (logical) Don't copy AST before generating CFG?
#' @param ssa (logical) Return CFG in SSA form?
#'
#' @return The control flow graph as a CFGraph object. The \code{[[} operator
#' can be used to extract individual basic blocks from the graph.
#'
#' @export

toCFG = function(ast, inPlace = FALSE, ssa = TRUE, insertReturn = TRUE) {
  UseMethod("toCFG")
}

#' @export
toCFG.Function =
function(ast, inPlace = FALSE, ssa = TRUE, insertReturn = TRUE) {
  # FIXME: Use a different parameter name?
  if (!inPlace)
    node = ast$copy()
  else
    node = ast

  if(insertReturn)
    node = insertReturn(node)

  cfg = ControlFlowGraph$new()
  builder = CFGBuilder$new(cfg)
  buildCFG(node$body, builder)

  # Always flow to the exit block.
  if (is.na(builder$insert_block))
    builder$insert_block = cfg$exit
  else if (builder$insert_block != cfg$exit)
    builder$create_br(cfg$exit)

  node$body = NULL
  node$cfg = cfg

  if (ssa)
    toSSA(node, inPlace = TRUE)

  node
}

#' @export
toCFG.ASTNode =
function(ast, inPlace = FALSE, ssa = TRUE, insertReturn = TRUE) {
  if (!inPlace)
    ast = ast$copy()

  # This node isn't a Function, so wrap it up in one.
  ast = Function$new(params = list(), body = ast)

  toCFG.Function(ast, inPlace = TRUE, ssa = ssa, insertReturn = insertReturn)
}

#' @export
toCFG.default =
function(ast, inPlace = FALSE, ssa = TRUE, insertReturn = TRUE) {
  ast = toAST(ast)
  toCFG(ast, inPlace = TRUE, ssa = ssa, insertReturn = insertReturn)
}


#' Build Basic Blocks from ASTNodes
#'
#' This helper function does a depth-first traversal of an AST in order to
#' build basic blocks for a CFG.
#'
#' Generally, this function should only be called from \code{toCFG()}.
#'
#' @param node (ASTNode) An ASTNode to build the graph from.
#' @param builder (CFGBuilder) The graph builder.
#'
buildCFG = function(node, builder) {
  # Don't do anything if no insert block is set.
  if (is.na(builder$insert_block))
    invisible (NULL)
  
  UseMethod("buildCFG")
}

#' @export
buildCFG.If = function(node, builder) {
  entry_t = builder$new_block()
  entry_f = builder$new_block()
  builder$create_cond_br(entry_t, entry_f, node$condition)

  # FIXME: 
  exit = builder$new_block()

  buildCFG(node$true, builder)
  # Flow to the exit if control didn't flow elsewhere.
  true_flows_to_exit = !is.na(builder$insert_block)
  if (true_flows_to_exit)
    builder$create_br(exit)

  builder$insert_block = entry_f
  if (!is.null(node$false))
    buildCFG(node$false, builder)

  if (!is.na(builder$insert_block)) # false flows to exit
    builder$create_br(exit)
  else if (true_flows_to_exit) # only true flows to exit
    builder$insert_block = exit
  else { # neither branch flows to exit
    # Delete the exit block and indicate that there is no control flow out of
    # this if-statement back to the caller (by leaving insert_block as NA).
    builder$remove_block(exit)
  }

  invisible (NULL)
}


#' @export
buildCFG.While = function(node, builder) {
  entry = builder$new_block()
  builder$create_br(entry)

  entry_b = builder$new_block()
  exit = builder$new_block()
  builder$create_cond_br(entry_b, exit, node$condition)

  # Push a new context so break/next flow to the correct place.
  builder$loop_push(entry, exit)

  buildCFG(node$body, builder)
  if (!is.na(builder$insert_block))
    builder$create_br(entry)

  builder$loop_pop()

  builder$insert_block = exit
  invisible (NULL)
}


#' @export
buildCFG.For = function(node, builder) {
  # Loop Setup (before entry)
  # =========================
  # Initialize the iterator.
  iterator_name = paste0("._iterator_", node$ivar$basename)
  iterator = Assign$new(Symbol$new(iterator_name), node$iter$copy())

  builder$append(iterator)

  # Initialize the counter and loop variable.
  counter_name = paste0("._counter_", node$ivar$basename)
  counter = Assign$new(Symbol$new(counter_name), Integer$new(1L))
  builder$append(counter)

  loop_var = Assign$new(
    Symbol$new(node$ivar$basename),
    Call$new("[[", list(Symbol$new(iterator_name), Symbol$new(counter_name)))
  )
  builder$append(loop_var)

  # Loop Entry
  # ==========
  entry = builder$new_block()
  builder$create_br(entry)

  # Advance the counter and loop variable.
  counter = Assign$new(
    Symbol$new(counter_name),
    Call$new("+", list(Symbol$new(counter_name), Integer$new(1L)))
  )
  builder$cfg[[entry]]$append(counter)
  builder$cfg[[entry]]$append(loop_var$copy())

  # Condition
  condition = Call$new("<=", list(
    Symbol$new(counter_name),
    Call$new("length", list(Symbol$new(iterator_name)))
  ))

  # Loop Body
  # =========
  entry_b = builder$new_block()
  exit = builder$new_block()
  builder$create_iter(entry_b, exit, condition, node$ivar, node$iter)

  # Push a new context so break/next flow to the correct place.
  builder$loop_push(entry, exit)

  buildCFG(node$body, builder)
  if (!is.na(builder$insert_block))
    builder$create_br(entry)

  builder$loop_pop()

  builder$insert_block = exit
  invisible (NULL)
}


#' @export
buildCFG.Break = function(node, builder) {
  builder$create_break()
  invisible (NULL)
}


#' @export
buildCFG.Next = function(node, builder) {
  builder$create_next()
  invisible (NULL)
}


#' @export
buildCFG.Return = function(node, builder) {
  # NOTE: We could keep the Return instead of creating a ._retval_ variable.

  val = node$args[[1]]

  # For returned assignments, return assigned variable. We could instead skip
  # the assignment altogether and just return the right-hand side.
  if (is(val, "Assign")) {
    val$parent = node$parent
    buildCFG(val, builder)
    val = val$write$copy()
  }

  assign = Assign$new(Symbol$new("._return_"), val)
  buildCFG(assign, builder)
  builder$create_ret()

  invisible (NULL)
}


#' @export
buildCFG.Brace = function(node, builder) {
  # Handle all subexpressions; they'll automatically be added to the graph.
  lapply(node$body, buildCFG, builder)
  invisible (NULL)
}


#' @export
buildCFG.Call = function(node, builder) {
  nodeApply(node, functionsToCFG, inPlace = TRUE)
  builder$append(node)
  invisible (NULL)
}

#' @export
buildCFG.Assign = buildCFG.Call
#' @export
buildCFG.Symbol = buildCFG.Call
#' @export
buildCFG.Literal = buildCFG.Call

# Function definitions don't change control flow, but still need to compute
# their CFGs.
#' @export
buildCFG.Function = buildCFG.Call


functionsToCFG = function(node, ...) {
  UseMethod("functionsToCFG")
}

#' @export
functionsToCFG.Function = function(node, ...) {
  toCFG.Function(node, inPlace = TRUE, ssa = FALSE)
  node
}

#' @export
functionsToCFG.default = function(node, ...) node
