#' @export
findPhiAssignVarNames =
function(cfg, ssa.graph = cfg$ssa$graph)
{
  phis = lapply(cfg$blocks, function(x) x$phi)
  w = sapply(phis, length) > 0

    # Find the p
  lapply(phis[w], function(x) lapply(x, function(x) getPhiAssign(x$write$name, cfg, ssa.graph)))
  
}

  

# If we had a reference to the Symbol in the read, we could mark that directly.


getPhiAssign =
function(sym, cfg, ssa.graph = cfg$ssa$graph)
{
  defs = igraph::neighbors(ssa.graph, sym, "in")
  as_ids(defs)
}
