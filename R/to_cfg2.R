
to_cfg2 = function(node) {
  # Need to make sure the node is a Function.
  if (!inherits(node, "Function")) {
    node = Function$new(params = list(), body = node)
  }

  # First linearize.
  node = linearize_blocks(node)

  # Now add links between blocks or generate a flow graph.
  make_flow_graph(node, NULL, NULL)

  node
}


make_flow_graph = function(node, helper, cfg) {
  UseMethod("make_flow_graph")
}


make_flow_graph.Function = function(node, helper, cfg) {
  blocks = node$body
  #if (is.list(node$body))
  #  blocks = node$body
  #else
  #  blocks = list(node$body)

  cfg = ControlFlowGraph2$new()

  helper = list(this_block = NULL, sib_block = cfg$exit,
    next_block = NULL, break_block = NULL)
  make_flow_graph(blocks, helper, cfg)

  node$cfg = cfg

  NULL
}

make_flow_graph.list = function(node, helper, cfg) {
  blocks = node

  # Add all the blocks as vertices in the graph. Then make the edges.
  ids = vapply(blocks, function(b) {
    id = cfg$add_vertex()
    cfg$blocks[[id]] = b
    id
  }, "")

  # Add an edge to the first block.
  if (is.null(helper$this_block))
    cfg$entry = ids[[1]]
  else
    cfg$add_edge(helper$this_block, ids[[1]])

  # Now process the blocks to add their outgoing edges.
  len = length(ids)
  for (i in seq_len(len - 1)) {
    new_helper = helper
    new_helper$this_block = ids[[i]]
    new_helper$sib_block = ids[[i + 1]]
    make_flow_graph(blocks[[i]], new_helper, cfg)
  }

  new_helper = helper
  new_helper$this_block = ids[[len]]
  make_flow_graph(blocks[[len]], new_helper, cfg)
}


make_flow_graph.Brace = function(node, helper, cfg) {
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

  # If last element is...
  #   ...If, then create branch in graph.
  #   ...For/While, then create loop in graph.
  #   ...Break/Next, then jump to appropriate loop block
  #   ...Return, then jump to exit block.

  len = length(node$body)
  if (len == 0)
    cfg$add_edge(helper$this_block, helper$sib_block)
  else
    make_flow_graph(node$body[[len]], helper, cfg)
}

make_flow_graph.For = function(node, helper, cfg) {
  # Create blocks for setup and header.
  #node$setup = Brace$new()
  #node$header = Brace$new()

  # Add the new blocks.
  # FIXME: These should be tracked on the For, not just in the CFG. See above.
  setup_block = cfg$add_vertex()
  cfg[[setup_block]] = Brace$new()

  header_block = cfg$add_vertex()
  cfg[[header_block]] = Brace$new()

  # Generate code for the header.
  # ...

  # Add edges into and out of setup.
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

  # Descend.
  make_flow_graph(node$body, helper, cfg)

  # Add edge out of header.
  cfg$add_edge(header_block, helper$break_block)

  NULL
}

make_flow_graph.If = function(node, helper, cfg) {
  # Process true branch, then false branch.
  # Each must flow into the sib_block (unless there is a return()).

  # Add edge from this_block to true block.
  make_flow_graph(node$true, helper, cfg)

  # Add edge from this_block to false block.
  if (is.null(node$false))
    cfg$add_edge(helper$this_block, helper$sib_block)
  else
    make_flow_graph(node$false, helper, cfg)

  NULL
}


# Edge-adding Cases ----------------------------------------
make_flow_graph.ASTNode = function(node, helper, cfg) {
  # Normal control flow.
  cfg$add_edge(helper$this_block, helper$sib_block)

  NULL
}

make_flow_graph.Break = function(node, helper, cfg) {
  cfg$add_edge(helper$this_block, helper$break_block)

  NULL
}

make_flow_graph.Next = function(node, helper, cfg) {
  cfg$add_edge(helper$this_block, helper$next_block)

  NULL
}

make_flow_graph.Return = function(node, helper, cfg) {
  # Link to exit block.
  cfg$add_edge(helper$this_block, cfg$exit)

  NULL
}

