#' @export
compute_cfg =
function(code) {
  UseMethod("compute_cfg")
}

#' @export
compute_cfg.BlockList =
function(code) {
  jumps = lapply(code$contents, function(block) {
    last = block[[length(block)]]
    get_jump(last, block$id)
  })
  jumps = do.call(rbind, jumps)
  # Use integer matrix so that igraph preserves numbering.
  jumps = apply(jumps, 2, as.integer)
  if (!is.matrix(jumps))
    jumps = matrix(jumps, ncol = 2)

  igraph::graph_from_edgelist(jumps)
}

#' @export
compute_cfg.Function =
function(code) {
  compute_cfg(code$body)
}

#' @export
compute_cfg.data.frame =
function(code) {
  tails = block_tails(code$block)
  jumps = Map(get_jump, code[tails, "line"], code[tails, "block"])
  jumps = do.call(rbind, jumps)
  igraph::graph_from_edgelist(jumps)
}


get_jump = function(node, block) {
  UseMethod("get_jump")
}

get_jump.If = function(node, block = NULL) {
  matrix(c(block, block, node$true$name, node$false$name), nrow = 2)
}

get_jump.Loop = function(node, block = NULL) {
  matrix(c(block, block, node$body$name, node$exit$name), nrow = 2)
}

get_jump.Branch = function(node, block = NULL) {
  matrix(c(block, node$target$name), nrow = 1)
}

get_jump.Symbol = function(node, block = NULL) {
  matrix(character(0), ncol = length(block) + 1)
}
