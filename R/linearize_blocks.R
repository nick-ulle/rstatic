CONTROL_FLOW = c("If", "For", "While", "Break", "Next", "Return")

# FIXME: What if an Assign contains a Brace, If, etc?

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

  if (!any(flows))
    return (node)

  blocks = split(node$body, cumsum(flows))

  blocks = lapply(blocks, function(b) {
    Brace$new(b, parent = node$parent)
  })

  names(blocks) = NULL
  blocks
}

#' @export
linearize_blocks.If = function(node) {
  node$true = linearize_blocks(node$true)
  if (!is.null(node$false))
    node$false = linearize_blocks(node$false)

  node
}

#' @export
linearize_blocks.Assign = function(node) {
  node
}

#' @export
linearize_blocks.Next = function(node) {
  node
}

#' @export
linearize_blocks.Break = function(node) {
  node
}

#' @export
linearize_blocks.Literal = function(node) {
  node
}
