#' @export
entry = function(node) {
  node$contents[[2L]]
}


#' @export
exit = function(node) {
  node$contents[[1L]]
}
