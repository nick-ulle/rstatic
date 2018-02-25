# FIXME: What if an Assign contains a Brace, If, etc?
# FIXME: What happens to braces within braces?
# NOTE: This does not visit subfunctions.

#' Split Blocks at Control Flow
#'
#' This function splits every block (Brace) in an AST after each control flow
#' expression. In the returned AST, a block can only have a control flow
#' expression as its final element.
#'
#' @param node (ASTNode) The root of the AST to be split.
#'
#' @export
split_blocks = function(node) {
  UseMethod("split_blocks")
}

#' @export
split_blocks.Brace = function(node) {
  node$body = lapply(node$body, split_blocks)

  # We need to put loops in their own block.
  flows = vapply(node$body, function(line) {
    c(is_control_flow(line), is_loop(line))
  }, logical(2))
  is_loop = flows[2, ]
  flows = flows[1, ]

  # Shift over by one element so each block starts after a flow.
  flows = c(FALSE, head(flows, -1))

  # Put each loop in its own block.
  flows[is_loop] = TRUE

  blocks = split(node$body, cumsum(flows))
  blocks = lapply(blocks, function(b) {
    Brace$new(b)
  })

  names(blocks) = NULL
  blocks
}

#' @export
split_blocks.If = function(node) {
  node$true = split_blocks(node$true)
  if (is.null(node$false))
    node$false = list()
  else
    node$false = split_blocks(node$false)

  node
}

#' @export
split_blocks.For = function(node) {
  node$body = split_blocks(node$body)

  node
}

split_blocks.While = split_blocks.For

#' @export
split_blocks.Function = function(node) {
  node$body = split_blocks(node$body)

  node
}

#' @export
split_blocks.ASTNode = function(node) node
