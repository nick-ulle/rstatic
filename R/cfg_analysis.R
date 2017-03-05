
#' Postorder Traversal of a Graph
#'
#' Given a graph, this function finds the order nodes are visited by a
#' postorder traversal.
#'
#' @param cfg (CFGraph) The graph on which to compute a traversal.
#' @param from (integer) A node index where the traversal should start.
#'
#' @return (integer) The node indexes in the order they'd be visited.
#'
#' @export
postorder = function(cfg, from = cfg$entry) {
  # Compute the postorder traversal.
  to_visit = Stack$new(type = "integer")
  to_visit$push(from)

  n = length(cfg)
  is_discovered = logical(n)
  visited = integer(n)
  n_visited = 0

  while (!to_visit$is_empty) {
    idx = to_visit$peek()
    is_discovered[[idx]] = TRUE

    succ = cfg[[idx]]$successors
    succ = succ[!is_discovered[succ]]

    if (length(succ) > 0) {
      # Undiscovered successors, so push them onto stack.
      to_visit$push_many(succ)
    } else {
      # No undiscovered successors, so visit and pop this node.
      to_visit$pop()
      n_visited = n_visited + 1
      visited[n_visited] = idx
    } # if

  } # while

  return (visited)
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
      } else if (inherits(node, "Replacement")) {
        preceeding_writes = union(preceeding_writes, node$args[[1]]$name)
      }
    }

    # Now have all writes, so record them in assign_blocks.
    assign_blocks[preceeding_writes] =
      lapply(assign_blocks[preceeding_writes], union, i)
  } # end for

  list (crossers, assign_blocks)
}

