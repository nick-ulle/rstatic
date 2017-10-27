
#' Postordering of a Graph
#'
#' Given a graph, this function finds a postordering of the nodes.
#'
#' A postordering gives nodes in the order they were last visited by a
#' depth-first search (as opposed to first visited). That is, the deepest nodes
#' appear first in the postordering. A postordering is not necessarily unique.
#'
#' For an acyclic graph, a reversed postordering is equivalent to a topological
#' sort.
#'
#' @param g (FlowGraph) The graph on which to compute a traversal.
#' @param from (integer) A node index where the traversal should start.
#'
#' @return (integer) The node indexes in the order they'd be visited.
#'
#' @export
postorder = function(g, from = g$entry) {
  dfs = igraph::dfs(g$graph, root = from, order = FALSE, order.out = TRUE)

  return (dfs$order.out)
}


#' Collect Cross-block Uses in a CFG
#'
#' This function collects the (unique) names of all variables that are used in
#' a different block than the block where they were defined.
#'
#' @param cfg (CFGraph) A control flow graph.
#'
#' @export
collect_crossblock_uses = function(cfg) {
  crossers = character(0)
  # Blocks where global symbols are assigned
  assign_blocks = list()

  for (i in seq_along(cfg)) {
    block = cfg[[i]]
    preceeding_writes = character(0)

    for (node in block$body) {
      # Get all reads that aren't preceeded by a write.
      reads = collect_reads(node)
      reads = setdiff(reads, preceeding_writes)
      crossers = union(crossers, reads)

      # Record all writes.
      if (inherits(node, "Assign")) {
        preceeding_writes = union(preceeding_writes, node$write$name)
      }
    }

    # Now have all writes, so record them in assign_blocks.
    assign_blocks[preceeding_writes] =
      lapply(assign_blocks[preceeding_writes], union, i)
  } # end for

  list (crossers, assign_blocks)
}

