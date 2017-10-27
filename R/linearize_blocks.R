CONTROL_FLOW = c("If", "For", "While", "Break", "Next", "Return")

# FIXME: What if an Assign contains a Brace, If, etc?
# FIXME: What happens to braces within braces?

#' @export
linearize_blocks = function(node) {
  UseMethod("linearize_blocks")
}

#' @export
linearize_blocks.Brace = function(node) {
  node$body = lapply(node$body, linearize_blocks)

  flows = vapply(node$body, function(x) {
    any(class(x) %in% CONTROL_FLOW)
  }, NA)
  # Shift over by one element so each block ends with a flow.
  flows = c(FALSE, head(flows, -1))

  if (any(flows)) {
    blocks = split(node$body, cumsum(flows))

    blocks = lapply(blocks, function(b) {
      Brace$new(b)
    })

  } else {
    # FIXME: Clean up this code.
    blocks = node
    return (node)
  }

  BlockList$new(blocks, parent = node$parent)
}

#' @export
linearize_blocks.If = function(node) {
  node$true = linearize_blocks(node$true)
  if (!is.null(node$false))
    node$false = linearize_blocks(node$false)

  node
}

#' @export
linearize_blocks.For = function(node) {
  node$body = linearize_blocks(node$body)

  node
}

#' @export
linearize_blocks.Function = function(node) {
  node$body = linearize_blocks(node$body)

  node
}

#' @export
linearize_blocks.ASTNode = function(node) node
