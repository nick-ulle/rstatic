
NONCONST = structure(list(), class = "NONCONST")
is_constant = function(x) {
  return (!is(x, "NONCONST"))
}

#' Sparse Conditional Constant Propagation
#'
#' This function applies sparse conditiona constant propagation to the
#' specified control flow graph.
#'
#' @param cfg (ControlFlowGraph) The graph on which to apply the algorithm.
#'
#' @return (list) A named list, with values for constant symbols and NONCONST
#' objects for nonconstant symbols.
#'
#' @references
#' Wegman, M. N., & Zadeck, F. K. (1991). Constant propagation with conditional
#' branches. ACM Transactions on Programming Languages and Systems (TOPLAS),
#' 13(2), 181-210.
#'
#' @export
propagate_constants = function(cfg) {
  helper = SCCHelper$new(cfg)

  # The entry block is always executed.
  const_visit_block(cfg$entry, helper)

  # First pass: traverse the CFG in approximate execution order.
  while (length(helper$flow_list) > 0) {
    edge = helper$flow_list[[1]]
    helper$flow_list = helper$flow_list[-1]

    if (!helper$executable[[edge]]) {
      helper$executable[[edge]] = TRUE

      dest = tail_of(cfg$graph, edge)

      # FIXME: First visit to block != first visit via this edge.
      const_visit_block(dest, helper)
    }
  }

  # Second pass: update expressions invalidated by cycles during the first
  # pass.
  # NOTE: The SSA list appears to ensure constants are correct when there are
  # cycles in the graph.
  while (length(helper$ssa_list) > 0) {
    edge = helper$ssa_list[[1]]
    helper$ssa_list = helper$ssa_list[-1]

    dest = tail_of(cfg$ssa$graph, edge)
    node = cfg$ssa[[as_ids(dest)]]
    # FIXME: Get name of block.
    b = node$parent$name
    in_edges = igraph::E(helper$cfg$graph)[to(b)]
    if ( any(helper$executable[in_edges]) ) {
      const_visit_node(node, helper)
    }
  } # end while

  return (helper$values)
}


# FIXME: Rename b?
const_visit_block = function(b, helper) {
  block = helper$cfg[[b]]
  # Compute the value of an expression.
  #
  # For an assignment, we need to look forwards (to uses) in the SSA graph. We
  # can find the assignment in the SSA graph by the SSA name it defines.
  #
  # NOTE: This is the only place SSA edges are added to worklist.

  for (phi in block$phi) {
    const_visit_node(phi, helper)
  }

  for (node in block$body) {
    const_visit_node(node, helper)
  }

  # Add outgoing edges (to next executed blocks) to flow worklist.
  trm = block$terminator
  if (is(trm, "BrTerminator")) {
    edge = igraph::E(helper$cfg$graph)[b %->% trm$dest]
    helper$flow_list = union(helper$flow_list, edge)

  } else if (is(trm, "CondBrTerminator")) {
    val = evalConst(trm$condition, helper)
    if (!is_constant(val)) {
      edge = igraph::E(helper$cfg$graph)[from(b)]
    } else if (val) {
      edge = igraph::E(helper$cfg$graph)[b %->% trm$true]
    } else {
      edge = igraph::E(helper$cfg$graph)[b %->% trm$false]
    }

    helper$flow_list = union(helper$flow_list, edge)

  } else if (is(trm, "RetTerminator")) {
    # pass
  } else {
    # FIXME:
    stop("unknown terminator for constant propagation.")
  }

  invisible (NULL)
}


const_visit_node = function(node, helper) {
  UseMethod("const_visit_node")
}


#' @export
const_visit_node.Assignment = function(node, helper) {
  name = node$write$name

  # Set constant in table.
  helper$values[[name]] = evalConst(node$read, helper)

  # Add SSA edges. This is redundant for expressions that appear later within
  # the same block, but checking for that case may be more expensive than
  # revisiting the expressions.
  #out = igraph::neighbors(helper$cfg$ssa$graph, name, "out")
  out_edges = igraph::E(helper$cfg$ssa$graph)[from(name)]
  helper$ssa_list = union(helper$ssa_list, out_edges)

  invisible (NULL)
}

#' @export
const_visit_node.Phi = function(node, helper) {
  # Compute meet of values of all operands to this phi-function.
  # Need to look backwards (to defs) in the SSA graph. We can find the phi in
  # the SSA graph by the SSA name it defines.

  # Get edges to check executability.
  b = node$parent$name
  edges = E(helper$cfg$graph)[node$blocks %->% b]
  is_exec = helper$executable[edges]
  defs = vapply(node$read[node$blocks[is_exec]], function(x) x$name,
    character(1))

  # Get definitions for values from executable edges.
  # TODO: Compute meet properly here.
  name = node$write$name
  vals = helper$values[defs]
  if (length(unique(vals)) == 1) {
    helper$values[[name]] = vals[[1]]
  } else {
    helper$values[[name]] = NONCONST
  }

  invisible (NULL)
}

#' @export
const_visit_node.Replacement = function(node, helper) {
  # FIXME: For now, don't update anything, since we completely ignore vectors
  # during constant analysis. Eventually this could update array SSA names.
  invisible (NULL)
}

#' @export
const_visit_node.default = function(node, helper) {
  browser()
}


# Evaluate a constant expression or return a sentinel value.
evalConst = function(node, helper) {
  UseMethod("evalConst")
}

#' @export
evalConst.Call = function(node, helper) {
  if (node$fn$name %in% c(">", "<", ">=", "<=", "==", "length", ":",
      "-", "+", "*", "/")
  ) {
    # TODO: Avoid unnecessary evaluation by collecting reads first and checking
    # that there are known constant values for all symbols.
    args = lapply(node$args, evalConst, helper)
    if ( all(vapply(args, is_constant, logical(1))) ) {
      val = do.call(node$fn$name, args)
      return (val)
    }
  }

  return (NONCONST)
}

#' @export
evalConst.Symbol = function(node, helper) {
  idx = match(node$name, names(helper$values))
  if (is.na(idx))
    return (NONCONST)

  return (helper$values[[idx]])
}
 
#' @export
evalConst.Brace = function(node, helper) {
  val = lapply(node$body, evalConst, helper)

  return (val[[length(node$body)]])
}

#' @export
evalConst.Literal = function(node, helper) {
  return (node$value)
}


SCCHelper = R6::R6Class("SCCHelper",
  "public" = list(
    cfg = NULL,
    executable = NULL,
    flow_list = integer(),
    ssa_list = integer(),
    values = list(),

    initialize = function(cfg) {
      self$cfg = cfg
      self$executable = logical(igraph::ecount(cfg$graph))

      return (self)
    }
  )
)
