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

# FIXME: Now body code always needs to be inside a Brace/Block.

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

build_cfg2.list = function(node, helper, cfg) {
  # Add all the blocks as vertices in the graph. Then make the edges.
  ids = vapply(node, function(b) {
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
    build_cfg2(node[[i]], new_helper, cfg)
  }

  new_helper = helper
  new_helper$this_block = ids[[len]]
  build_cfg2(node[[len]], new_helper, cfg)
}


build_cfg2.Brace = function(node, helper, cfg) {
  lapply(node$body, recurse_to_cfg)

  # Since blocks are linear, only the final expression affects control flow.
  len = length(node$body)
  if (len > 0)
    build_cfg2(node$body[[len]], helper, cfg)
  else
    # Empty block.
    cfg$add_edge(helper$this_block, helper$sib_block)
    # NOTE: Could look for functions here.
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
  # Normal control flow.
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


recurse_to_cfg = function(node) {
  UseMethod("recurse_to_cfg")
}

recurse_to_cfg.Function = function(node) {
  to_cfg2.Function(node, in_place = TRUE, linearize = FALSE)
}

recurse_to_cfg.list = function(node) {
  lapply(node, function(block) {
    recurse_to_cfg(block$body)
  })
}

recurse_to_cfg.Call = function(node) {
  lapply(node$args, recurse_to_cfg)

  recurse_to_cfg(node$fn)

  node
}

recurse_to_cfg.Assign = function(node) {
  recurse_to_cfg(node$read)
  recurse_to_cfg(node$write)

  node
}

recurse_to_cfg.ASTNode = function(node) {
  node
}
