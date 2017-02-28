
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

