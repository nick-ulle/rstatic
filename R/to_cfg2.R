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

#' @export
to_cfg2 = function(node, in_place = FALSE, linearize = TRUE) {
  UseMethod("to_cfg2")
}

#' @export
to_cfg2.Function = function(node, in_place = FALSE, linearize = TRUE) {
  if (!in_place)
    node = node$copy()

  if (linearize)
    node = linearize_blocks(node)

  cfg = ControlFlowGraph2$new()
  helper = list(
    this_block = NULL, sib_block = cfg$exit,
    next_block = NULL, break_block = NULL)
  build_cfg2(node$body, helper, cfg)

  node$cfg = cfg

  node
}

#' @export
to_cfg2.ASTNode = function(node, in_place = FALSE, linearize = TRUE) {
  if (!in_place)
    node = node$copy()

  # This node isn't a Function, so wrap it up in one.
  node = Function$new(params = list(), body = node)

  to_cfg2.Function(node, in_place = TRUE, linearize = linearize)
}

#' @export
to_cfg2.default =
function(node, in_place = FALSE, linearize = TRUE) {
  node = to_ast(node)
  to_cfg2(node, in_place = TRUE, linearize = linearize)
}



build_cfg2 = function(node, helper, cfg) {
  UseMethod("build_cfg2")
}

build_cfg2.BlockList = function(node, helper, cfg) {
  # Add all the blocks in the list to the graph.
  siblings = vapply(node$body, function(block) {
    block$id = cfg$add_block(block)
  }, "")

  # Add parent sibling block as last sibling block.
  len = length(siblings)
  siblings = c(siblings[2:len], helper$sib_block)

  # Now process the blocks to add their outgoing edges.
  for (i in seq_along(siblings)) {
    new_helper = helper
    if (i > 1)
      new_helper$this_block = NULL
    new_helper$sib_block = siblings[[i]]
    build_cfg2(node[i], new_helper, cfg)
  }

  NULL
}

build_cfg2.Brace = function(node, helper, cfg) {
  if (!inherits(node$parent, "BlockList"))
    node$id = cfg$add_block(node)

  # When parent is If, For, or While: this_block is not NULL and an incoming
  # edge (e.g., If -> Block) must be added since the block was just added to
  # the graph.
  if (!is.null(helper$this_block))
    cfg$add_edge(helper$this_block, node$id)

  helper$this_block = node$id

  # Check for function definitions.
  lapply(node$body, nested_functions_to_cfg)

  # Only the final expression affects control flow.
  len = length(node$body)
  if (len > 0)
    build_cfg2(node$body[[len]], helper, cfg)
  else
    # Empty block.
    cfg$add_edge(helper$this_block, helper$sib_block)

  NULL
}


build_cfg2.If = function(node, helper, cfg) {
  # Process true branch, then false branch.

  build_cfg2(node$true, helper, cfg)

  if (is.null(node$false))
    # No false branch, so exit to sibling block.
    cfg$add_edge(helper$this_block, helper$sib_block)
  else
    build_cfg2(node$false, helper, cfg)

  NULL
}


build_cfg2.While = function(node, helper, cfg) {
  # Create block for header (a while-loop does not need a setup block).
  node$header = Brace$new()

  header_block = cfg$add_vertex()
  cfg[[header_block]] = node$header

  # FIXME: Generate code for the header.
  # ...

  cfg$add_edge(helper$this_block, header_block)

  # Set
  #   break_block = sib_block (the original)
  #   next_block = header
  #   sib_block = header (because this is where the last body block will go)
  #   this_block = header (because this is where we enter from)
  helper$break_block = helper$sib_block
  helper$next_block = header_block
  helper$sib_block = header_block
  helper$this_block = header_block

  build_cfg2(node$body, helper, cfg)

  # Add edge to exit loop.
  cfg$add_edge(header_block, helper$break_block)

  NULL
}

build_cfg2.For = function(node, helper, cfg) {
  # Create blocks for setup and header.
  node$setup = Brace$new()
  node$header = Brace$new()

  setup_block = cfg$add_vertex()
  cfg[[setup_block]] = node$setup

  header_block = cfg$add_vertex()
  cfg[[header_block]] = node$header

  # FIXME: Generate code for these blocks.
  # ...

  cfg$add_edge(helper$this_block, setup_block)
  cfg$add_edge(setup_block, header_block)

  # Set
  #   break_block = sib_block (the original)
  #   next_block = header
  #   sib_block = header (because this is where the last body block will go)
  #   this_block = header (because this is where we enter from)
  helper$break_block = helper$sib_block
  helper$next_block = header_block
  helper$sib_block = header_block
  helper$this_block = header_block

  build_cfg2(node$body, helper, cfg)

  # Add edge to exit loop.
  cfg$add_edge(header_block, helper$break_block)

  NULL
}


# Edge-adding Cases ----------------------------------------
build_cfg2.ASTNode = function(node, helper, cfg) {
  # In this case, control ascends from the current control structure.
  cfg$add_edge(helper$this_block, helper$sib_block)

  NULL
}

build_cfg2.Break = function(node, helper, cfg) {
  cfg$add_edge(helper$this_block, helper$break_block)

  NULL
}

build_cfg2.Next = function(node, helper, cfg) {
  cfg$add_edge(helper$this_block, helper$next_block)

  NULL
}

build_cfg2.Return = function(node, helper, cfg) {
  # Link to exit block.
  cfg$add_edge(helper$this_block, cfg$exit)

  NULL
}


nested_functions_to_cfg = function(node) {
  UseMethod("nested_functions_to_cfg")
}

nested_functions_to_cfg.Function = function(node) {
  to_cfg2.Function(node, in_place = TRUE, linearize = FALSE)
}

nested_functions_to_cfg.Call = function(node) {
  lapply(node$args, nested_functions_to_cfg)

  nested_functions_to_cfg(node$fn)

  node
}

nested_functions_to_cfg.Assign = function(node) {
  nested_functions_to_cfg(node$read)
  nested_functions_to_cfg(node$write)

  node
}

nested_functions_to_cfg.ASTNode = function(node) {
  # Skip over everything else. Blocks in If, For, and While are visited by
  # build_cfg2(), so don't visit them again here.
  node
}
