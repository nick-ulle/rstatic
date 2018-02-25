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
#' @param linearize (logical) Apply \code{split_blocks()} to the node
#' before generating the CFG?
#'
#' @return A Function node with the control flow graph in its \code{$cfg}
#' field.
#'
#' @export
to_cfg =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE,
  linearize = TRUE)
{
  UseMethod("to_cfg")
}

#' @export
to_cfg.Function =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE,
  linearize = TRUE)
{
  if (!in_place)
    node = node$copy()

  if (insert_return)
    node = insert_return(node)

  if (linearize)
    node = split_blocks(node)

  cfg = ControlFlowGraph$new(node)
  helper = list(
    this_block = NULL, sib_block = cfg$exit,
    next_block = NULL, break_block = NULL)
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
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE,
  linearize = TRUE)
{
  if (!in_place)
    node = node$copy()

  if (!is(node, "Brace"))
    node = Brace$new(node)

  # This node isn't a Function, so wrap it up in one.
  node = Function$new(params = list(), body = node)

  to_cfg.Function(node, in_place = TRUE, ssa, insert_return, linearize)
}

#' @export
to_cfg.default =
function(node, in_place = FALSE, ssa = TRUE, insert_return = TRUE,
  linearize = TRUE)
{
  node = to_ast(node)
  to_cfg(node, in_place = TRUE, ssa, insert_return, linearize)
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


build_cfg = function(node, helper, cfg) {
  UseMethod("build_cfg")
}

# The problem is that .list handles figuring out siblings, but we need to
# grab the id of the first block for true/false in .If

build_cfg.list = function(node, helper, cfg) {
  # Add all the blocks in the list to the graph.
  siblings = vapply(node, function(block) {
    block$id = cfg$add_block(block)
  }, "")

  # Add parent sibling block as last sibling block.
  len = length(siblings)
  entry = siblings[1]
  siblings = c(siblings[-1], helper$sib_block)

  # Now process the blocks to add their outgoing edges.
  for (i in seq_along(siblings)) {
    new_helper = helper
    if (i > 1)
      new_helper$this_block = NULL
    new_helper$sib_block = siblings[[i]]
    build_cfg.Brace(node[[i]], new_helper, cfg)
  }

  return (entry)
}


#' @export
build_cfg.Brace = function(node, helper, cfg) {
  # FIXME: Try to add incoming edges at the source block rather than the
  # destination block.
  # When parent is If, For, or While: this_block is not NULL and an incoming
  # edge (e.g., If -> Block) must be added since the block was just added to
  # the graph.
  #if (!is.null(helper$this_block))
  #  cfg$add_edge(helper$this_block, node$id)

  helper$this_block = node$id

  # Check for function definitions.
  lapply(node$body, nested_functions_to_cfg)

  # Only the final expression affects control flow.
  len = length(node$body)
  if (len > 0)
    build_cfg(node$body[[len]], helper, cfg)
  else
    # Empty block.
    cfg$add_edge(helper$this_block, helper$sib_block)

  NULL
}


#' @export
build_cfg.If = function(node, helper, cfg) {
  # Process true branch, then false branch.
  # NOTE: Edges get added when the component blocks are converted, not here.
  # In other words, see the .Brace method.

  # We need to do a few things here:
  #   1) Get ids for the true and false blocks, so the labels can be added to
  #      the conditional branch at the end of this block.
  #   2) Mark the end of each subgraph
  # 
  # The helper IS the pointer into the CFG. The tricky thing is that blocks are
  # created in .Brace, so edges get added when `this block` is the child.
  #
  # We could create the block here and update `this_block`. Then construct the
  # rest of the subgraph recursively. This is ugly since we have to handle the
  # first block on each subgraph specially.

  # Add all the blocks in the list to the graph.
  id_true = build_cfg.list(node$true, helper, cfg)
  cfg$add_edge(helper[["this_block"]], id_true)
  node$true = id_true

  # Same thing for false block, but check that there's actually something
  # inside. NOTE: We could have empty false blocks have list(Brace) so we don't
  # need to do this.
  if (length(node$false) > 0)
    id_false = build_cfg.list(node$false, helper, cfg)
  else # exit to sibling block.
    id_false = helper[["sib_block"]]

  cfg$add_edge(helper[["this_block"]], id_false)
  node$false = id_false

  NULL
}

#' @export
build_cfg.While = function(node, helper, cfg) {
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

  id_body = build_cfg.list(node$body, helper, cfg)
  cfg$add_edge(helper[["this_block"]], id_body)
  node$body = id_body

  # Add edge to exit loop.
  cfg$add_edge(helper[["this_block"]], helper[["break_block"]])

  NULL
}

#' @export
build_cfg.For = function(node, helper, cfg) {
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

  id_body = build_cfg.list(node$body, helper, cfg)
  cfg$add_edge(helper[["this_block"]], id_body)
  node$body = id_body

  # Add edge to exit loop.
  cfg$add_edge(helper[["this_block"]], helper[["break_block"]])

  NULL
}


# Edge-adding Cases ----------------------------------------
#' @export
build_cfg.ASTNode = function(node, helper, cfg) {
  # In this case, control ascends from the current control structure.
  cfg$add_edge(helper$this_block, helper$sib_block)

  NULL
}

#' @export
build_cfg.Break = function(node, helper, cfg) {
  if (is.null(helper$break_block))
    warning(sprintf(
      'invalid use of Break. No outgoing edge will be added for block "%s".',
      helper$this_block
    ))
  else
    cfg$add_edge(helper$this_block, helper$break_block)

  NULL
}

#' @export
build_cfg.Next = function(node, helper, cfg) {
  if (is.null(helper$next_block))
    warning(sprintf(
      'invalid use of Next. No outgoing edge will be added for block "%s".',
      helper$this_block
    ))
  else
    cfg$add_edge(helper$this_block, helper$next_block)

  NULL
}

#' @export
build_cfg.Return = function(node, helper, cfg) {
  # Link to exit block.
  cfg$add_edge(helper$this_block, cfg$exit)

  NULL
}


nested_functions_to_cfg = function(node) {
  UseMethod("nested_functions_to_cfg")
}

#' @export
nested_functions_to_cfg.Function = function(node) {
  to_cfg.Function(node, in_place = TRUE, ssa = FALSE, insert_return = FALSE,
    linearize = TRUE)
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
