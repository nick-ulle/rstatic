#' Get Entry Block
#'
#' This function gets the entry block from a Function or BlockList.
#'
#' @param node (Function | BlockList) An object which contains Blocks.
#'
#' @return The entry block.
#'
#' @export
entry_block =
function(node)
{
  UseMethod("entry_block")
}


#' @export
entry_block.BlockList =
function(node)
{
  node$contents[[node$entry_index]]
}


#' @export
entry_block.Function =
function(node)
{
  entry_block(node$body)
}


#' Get Exit Block
#'
#' This function gets the exit_block from a Function or BlockList.
#'
#' @param node (Function | BlockList) An object which contains Blocks.
#'
#' @return The exit block.
#'
#' @export
exit_block =
function(node)
{
  UseMethod("exit_block")
}


#' @export
exit_block.BlockList =
function(node)
{
  node$contents[[node$exit_index]]
}


#' @export
exit_block.Function =
function(node)
{
  exit_block(node$body)
}
