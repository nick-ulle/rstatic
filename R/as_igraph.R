

#' Convert CFGraph to igraph
#'
#' This function converts a CFGraph object to an igraph object, which is useful
#' for plotting the graph. The resulting igraph does not retain the code inside
#' the basic blocks, only the structure of the original CFGraph.
#'
#' @param cfg (CFGraph) a graph to convert
#'
#' @export
as_igraph = function(cfg) {
  if (!requireNamespace("igraph", quietly = TRUE))
    stop('This function requires the "igraph" package. Please install it.')

  # Get edge list.
  edges = lapply(seq_along(cfg$blocks),
    function(i) {
      pred = cfg[[i]]$predecessors
      if (length(pred) == 0)
        return (NULL)

      cbind(pred, i)
    }
  )
  edges = do.call(rbind, edges)

  return (igraph::graph_from_edgelist(edges, directed = TRUE))
}
