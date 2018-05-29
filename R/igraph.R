# Utility functions for working with igraph graphs.

#' Dominance Frontiers of a Graph
#'
#' This function computes the dominance frontiers for a graph.
#'
#' The dominance frontier for a block \eqn{b} is the set of all blocks \eqn{y}
#' such that \eqn{b} dominates a predecessor of \eqn{y} but does not strictly
#' dominate \eqn{y}. In other words, the dominance frontier for \eqn{b} is the
#' set of blocks immediately beyond the blocks dominated by \eqn{b}, where
#' control-flow merges from a separate part of the program.
#'
#' @param g (igraph) The graph.
#' @param dom_t (igraph) The dominator tree for the graph.
#'
#' @return A graph with a vertex for each vertex in the original graph. Each
#' vertex has edges to all vertices in its dominance frontier.
#'
#' @references
#' Cooper, K. D., Harvey T. J., and Kennedy, K. (2001) A simple, fast dominance
#' algorithm. Software Practice & Experience 4, 1-10.
#'
#' Cooper, K. D. and Torczon, L. (2012) Engineering a Compiler. Elsevier.
#'
#' @export
dominance_frontiers = function(g, dom_t = dominator_tree(g)) {
  # For each node where control flow merges...
  merges = igraph::V(g)[ igraph::degree(g, mode = "in") >= 2 ]

  edges = lapply(merges, function(m) {
    # The entry node is the parent of every other node in the dominator tree,
    # so there's always a parent here.
    parent = igraph::neighbors(dom_t, m, "in")

    # ...start from each of the node's predecessors in the graph...
    g_preds = igraph::neighbors(g, m, "in")
    edges = lapply(g_preds, function(pred) {
      # ...and ascend to the node's dominator tree parent.
      steps = igraph::subcomponent(dom_t, pred, "in")
      steps[cumsum(steps == parent) == 0]
    })
    edges = unlist(edges, recursive = FALSE, use.names = FALSE)

    rbind(edges, m, deparse.level = 0)
  })
  edges = do.call(cbind, edges)

  len = igraph::vcount(g)
  igraph::make_empty_graph(len) + igraph::edges(edges)
}


#' Dominator Tree of a Graph
#'
#' This function computes the dominator tree for a graph.
#'
#' This is a utility function to make working with igraph easier.
#'
#' A block \eqn{b_i} dominates another block \eqn{b_j} when all paths to
#' \eqn{b_j} must pass through \eqn{b_i}. The closest dominator of a block
#' (excluding the block itself) is called its immediate dominator. In the
#' dominator tree, each block is a child of its immediate dominator.
#'
#' @param g (igraph) The graph.
#' @param root (integer) Index of the root node of the graph.
#'
#' @return The dominator tree as an igraph graph.
#'
#' @export
dominator_tree = function(g, root = 2) {
  igraph::dominator_tree(g, root)[["domtree"]]
}


#' Vertex Successors
#'
#' This function gets the successors of a vertex in a graph.
#'
#' This is a utility function to make working with igraph easier.
#'
#' @param g (igraph) The graph.
#' @param id An index into the graph, or a Block.
#'
#' @export
successors = function(g, id) {
  if (is(id, "Block"))
    id = block$id

  igraph::neighbors(g, id, "out")
}


#' Vertex Predecessors
#'
#' This function gets the predecessors of a vertex in a graph.
#'
#' This is a utility function to make working with igraph easier.
#'
#' @param g (igraph) The graph.
#' @param id An index into the graph, or a Block.
#'
#' @export
predecessors = function(g, id) {
  if (is(id, "Block"))
    id = block$id

  igraph::neighbors(g, id, "in")
}


#' Postorder of a Graph
#'
#' Given a graph, this function returns the order the vertices are exited in a
#' depth-first traversal.
#'
#' A postorder is not always unique. For an acyclic graph, reversed postorder
#' is equivalent to a topological sort.
#'
#' @param g (igraph) The graph.
#' @param root (integer) A vertex where the traversal should start.
#'
#' @return (integer) A postorder of the graph.
#'
#' @export
postorder = function(g, root) {
  igraph::dfs(g, root = root, order = FALSE, order.out = TRUE)[["order.out"]]
}
