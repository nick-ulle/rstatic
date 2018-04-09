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
quote_cfg = function(expr, ...) {
  ast = to_ast(substitute(expr))
  to_cfg(ast, in_place = TRUE, ...)
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
to_cfg =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE)
{
  UseMethod("to_cfg")
}

#' @export
to_cfg.Function =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE)
{
  if (!in_place)
    node = node$copy()

  if (insert_return)
    node = insert_return(node)

  cfg = ControlFlowGraph$new(node)
  helper = c(
    this_block = NA, sib_block = cfg$exit,
    next_block = NA, break_block = NA)
  build_cfg(node$body, helper, cfg)

  # Sort the blocks in reverse postorder to make them easier to read and ensure
  # SSA numbers will increase monotonically.
  ordering = rev(postorder(cfg))
  cfg$reorder(ordering)

  node$cfg = cfg

  if (ssa)
    to_ssa(node)

  node
}

#' @export
to_cfg.ASTNode =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE)
{
  if (!in_place)
    node = node$copy()

  if (!is(node, "Brace"))
    node = Brace$new(node)

  # This node isn't a Function, so wrap it up in one.
  node = Function$new(params = list(), body = node)

  to_cfg.Function(node, in_place = TRUE, ssa, insert_return)
}

#' @export
to_cfg.default =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE)
{
  node = to_ast(node)
  to_cfg(node, in_place = TRUE, ssa, insert_return)
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


build_cfg = function(node, helper, cfg, depth) {
  UseMethod("build_cfg")
}


build_cfg.Brace = function(node, helper, cfg, depth = 1L) {
  # Split into blocks and add them to the graph.
  blocks = split_blocks(node)
  siblings = vapply(blocks, function(block) {
    block$id = cfg$add_block(block)
  }, "")

  # After last block, branch to parent's sibling block.
  len = length(siblings)
  entry = siblings[[1]]
  siblings = c(siblings[-1], helper[["sib_block"]])

  # Build the subgraph for each block.
  for (i in seq_along(siblings)) {
    helper[["sib_block"]] = siblings[[i]]
    build_cfg.Block(blocks[[i]], helper, cfg, depth)
    if (i == 1)
      helper[["this_block"]] = NA
  }

  entry
}


#' @export
build_cfg.Block = function(node, helper, cfg, depth = 1L) {
  helper[["this_block"]] = node$id
  node$depth = depth

  # Check for function definitions.
  lapply(node$body, nested_functions_to_cfg)

  # Only the final expression affects control flow.
  len = length(node$body)
  if (len > 0)
    build_cfg(node$body[[len]], helper, cfg, depth)
  else
    # Empty block.
    cfg$add_edge(helper[["this_block"]], helper[["sib_block"]])

  NULL
}

#' @export
build_cfg.If = function(node, helper, cfg, depth = 1L) {
  # Process true branch, then false branch.
  id_true = build_cfg.Brace(node$true, helper, cfg, depth + 1L)
  cfg$add_edge(helper[["this_block"]], id_true)
  node$true = id_true

  id_false = build_cfg.Brace(node$false, helper, cfg, depth + 1L)
  cfg$add_edge(helper[["this_block"]], id_false)
  node$false = id_false

  # Mark the blocks at the end of the if-statement.
  #exits = predecessors(cfg, helper[["sib_block"]])
  #vapply(cfg[exits], function(block) block$endif = TRUE, NA)
  #browser()

  NULL
}

#' @export
build_cfg.While = function(node, helper, cfg, depth = 1L) {
  # This *IS* the test block.

  #id_test = cfg$add_block()
  #node$test = cfg[[id_test]]

  ## NOTE: The test block should test the condition. The children of this block
  ## can be found in the CFG rather than on the generated If.
  ## FIXME: No way to distinguish true/false edge.
  #node$test[[1]] = If$new(node$condition$copy(), Brace$new())

  #cfg$add_edge(helper$this_block, id_test)

  # Set
  #   break_block = sib_block (the original)
  #   next_block = test
  #   sib_block = test (because this is where the last body block will go)
  #   this_block = test (because this is where we enter from)
  helper[["break_block"]] = helper[["sib_block"]]
  helper[["next_block"]]  = helper[["this_block"]]
  helper[["sib_block"]]   = helper[["this_block"]]

  id_body = build_cfg.Brace(node$body, helper, cfg, depth + 1L)
  cfg$add_edge(helper[["this_block"]], id_body)
  node$body = id_body

  # Add edge to exit loop.
  cfg$add_edge(helper[["this_block"]], helper[["break_block"]])
  node$exit = helper[["break_block"]]

  NULL
}

#' @export
build_cfg.For = function(node, helper, cfg, depth = 1L) {
  #id_setup = cfg$add_block()
  #node$setup = cfg[[id_setup]]

  #id_test = cfg$add_block()
  #node$test = cfg[[id_test]]

  #id_increment = cfg$add_block()
  #node$increment = cfg[[id_increment]]

  # Generate code for the setup, test, and increment blocks.
  #
  #   for (x in xs) {...}
  #
  # becomes
  #
  #   # %setup
  #   i = 1
  #
  #   repeat {
  #     # %test
  #     if (i > length(xs)) break
  #
  #     # %increment
  #     x = xs[[i]]
  #     i = i + 1
  #
  #     # %body
  #     ...
  #   }

  #counter = Symbol$new(paste0("._counter_", node$ivar$basename))
  #node$setup[[1]] = Assign$new(counter, Integer$new(1L))

  #if (is(node$iter, "Call")) {
  #  iterator = Symbol$new(paste0("._iterator_", node$ivar$basename))
  #  node$setup[[2]] = Assign$new(iterator, node$iter$copy())
  #} else {
  #  iterator = node$ivar
  #}

  # NOTE: Technically the length could also be computed just once, in the setup
  # block.

  # NOTE: The test block should test the condition. The children of this block
  # can be found in the CFG rather than on the generated If.
  # FIXME: No way to distinguish true/false edge.
  #node$test[[1]] = If$new(
  #  Call$new("<=", list(counter$copy(),
  #      Call$new("length", list(iterator$copy()))) ),
  #  Brace$new() )

  #loop_var = Symbol$new(node$ivar$basename)
  #node$increment[[1]] = Assign$new(loop_var,
  #  Subset$new("[[", list(iterator$copy(), counter$copy())) )
  #node$increment[[2]] = Assign$new(counter$copy(),
  #  Call$new("+", list(counter$copy(), Integer$new(1L))) )

  # --------------------

  #cfg$add_edge(helper$this_block, id_setup)
  #cfg$add_edge(id_setup, id_test)
  #cfg$add_edge(id_test, id_increment)

  # Set
  #   break_block = sib_block (the original)
  #   next_block = test
  #   sib_block = test (because this is where the last body block will go)
  #   this_block = increment (because this is where we enter from)
  helper[["break_block"]] = helper[["sib_block"]]
  helper[["next_block"]]  = helper[["this_block"]]
  helper[["sib_block"]]   = helper[["this_block"]]

  id_body = build_cfg.Brace(node$body, helper, cfg, depth + 1L)
  cfg$add_edge(helper[["this_block"]], id_body)
  node$body = id_body

  # Add edge to exit loop.
  cfg$add_edge(helper[["this_block"]], helper[["break_block"]])
  node$exit = helper[["break_block"]]

  NULL
}


# Edge-adding Cases ----------------------------------------
#' @export
build_cfg.ASTNode = function(node, helper, cfg, depth = 1L) {
  # In this case, control ascends from the current control structure.
  cfg$add_edge(helper[["this_block"]], helper[["sib_block"]])

  NULL
}

#' @export
build_cfg.Break = function(node, helper, cfg, depth = 1L) {
  if (is.na(helper[["break_block"]]))
    warning(sprintf(
      'invalid use of Break. No outgoing edge will be added for block "%s".',
      helper[["this_block"]]
    ))
  else
    cfg$add_edge(helper[["this_block"]], helper[["break_block"]])

  NULL
}

#' @export
build_cfg.Next = function(node, helper, cfg, depth = 1L) {
  if (is.na(helper[["next_block"]]))
    warning(sprintf(
      'invalid use of Next. No outgoing edge will be added for block "%s".',
      helper[["this_block"]]
    ))
  else
    cfg$add_edge(helper[["this_block"]], helper[["next_block"]])

  NULL
}

#' @export
build_cfg.Return = function(node, helper, cfg, depth = 1L) {
  # Link to exit block.
  cfg$add_edge(helper[["this_block"]], cfg$exit)

  NULL
}


nested_functions_to_cfg = function(node) {
  UseMethod("nested_functions_to_cfg")
}

#' @export
nested_functions_to_cfg.Function = function(node) {
  to_cfg.Function(node, in_place = TRUE, ssa = FALSE, insert_return = FALSE)
}

#' @export
nested_functions_to_cfg.Call = function(node) {
  lapply(node$args, nested_functions_to_cfg)

  nested_functions_to_cfg(node$fn)

  node
}

#' @export
nested_functions_to_cfg.Assign = function(node) {
  nested_functions_to_cfg(node$read)
  nested_functions_to_cfg(node$write)

  node
}

#' @export
nested_functions_to_cfg.ASTNode = function(node) {
  # Skip over everything else. Blocks in If, For, and While are visited by
  # build_cfg(), so don't visit them again here.
  node
}


split_blocks = function(node) {
  #node$body = lapply(node$body, split_blocks)

  flows = vapply(node$body, function(line) {
    c(is_control_flow(line), is_loop(line))
  }, logical(2))
  is_loop = flows[2, ]
  flows = flows[1, ]

  # Shift over by one element so each block starts after a flow.
  flows = c(FALSE, head(flows, -1))

  # Put each loop in its own block.
  flows[is_loop] = TRUE

  blocks = split(node$body, cumsum(flows))
  blocks = lapply(blocks, function(b) {
     # Could be Block$new(b)
    Block$new(b)
  })

  names(blocks) = NULL
  blocks
}
