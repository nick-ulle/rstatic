
#' @include ast_node.R
NULL

#' Convert CFG Code to Static Single-Assignment Form
#'
#' This function converts code in a control flow graph (CFG) to static
#' single-assignment form.
#'
#' @param cfg (CFGraph) A control flow graph.
#'
#' @return The control flow graph as a CFGraph object, with the code in each
#' block converted to SSA form.
#'
#' @export
ssa = function(cfg) {
  # TODO: make this function's implementation more idiomatic.
  dom_t = dom_tree(cfg)
  dom_f = dom_frontier(cfg, dom_t)

  globals = character(0) # symbols used in more than one block
  assign_blocks = list() # blocks where global symbols are assigned

  for (i in seq_along(cfg)) {
    block = cfg[[i]]
    varkill = character(0)

    for (node in block$body) {
      # TODO: Ignoring all but assignments may skip some reads; do we need to
      # add these reads to the globals set?
      if (!is(node, "Assign"))
        next

      # Add all read variables not in varkill to the globals set.
      reads = collect_reads(node$read)
      reads = setdiff(reads, varkill)
      globals = union(globals, reads)

      # Add write variable to the kill set and add current block to its blocks
      # set.
      # FIXME: Does __retval__ need to be ignored?
      name = node$write$name
      varkill = union(varkill, name)

      # Check that assign_blocks[[name]] exists.
      if (is.null(assign_blocks[[name]])) {
        assign_blocks[[name]] = i
      } else {
        assign_blocks[[name]] = union(assign_blocks[[name]], i)
      }
    }
  } # end for

  # Insert phi-functions.
  for (name in globals) {
    # Add phi-function to dominance frontier for each block with an assignment.
    worklist = assign_blocks[[name]]
    for (b in worklist) {
      for (d in dom_f[[b]]) {
        if (has_phi(cfg[[d]], name))
          next

        phi = Phi$new(Symbol$new(name))
        cfg[[d]]$append(phi, 0L)
        worklist = union(worklist, d)
      } # end for d
    }
  } # end for name

  # Rename variables.

  return (cfg)
}


# FIXME: This doesn't count function names as reads.
collect_reads = function(node) {
  UseMethod("collect_reads")
}

collect_reads.Call = function(node) {
  names = lapply(node$args, collect_reads)
  return (unique(unlist(names)))
}

collect_reads.Symbol = function(node) {
  return (node$name)
}

collect_reads.Literal = function(node) {
  return (character(0))
}
