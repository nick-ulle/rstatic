
#' Compute SSA Graph
#'
#' This generic function computes the static single-assignment (SSA) graph for
#' the given code.
#'
#' Nodes in the SSA graph represent expressions that define or use variables,
#' while edges represent data flows from definitions to uses. In SSA form, each
#' variable name corresponds to a single program value, so the SSA graph is
#' sparse compared to the def-use graph.
#'
compute_ssag =
function(code) {
  UseMethod("compute_ssag")
}

compute_ssag.BlockList =
function(node) {
  # Find all nodes that contain variables.
  # Don't call contains_symbol until we reach the expression level.
  nodes = find_nodes(node, function(node, ...) {
    if (inherits(node, "Block"))
      FALSE
    else
      contains_symbol(node, ...)
  }, recursive = FALSE)

  # Now we have the expressions that give rise to the nodes. Need to determine
  # the variables defined and used by each expression to create the edges.
  browser()
}

contains_symbol = function(node, ...) {
  # FIXME: Inefficient to use find_nodes; add function that finds first node.
  # Check if node contains a symbol.
  is(node, "Symbol") || ( length(find_nodes(node, is, "Symbol")) > 0 )
}
