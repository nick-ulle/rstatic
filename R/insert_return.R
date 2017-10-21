
# Checks to see if we need to enclose the final expression
# within a call to return()
#
# insert_return(quote(return(x + 1))  )
# insert_return(quote(x + 1))
# insert_return(quote({ x = 2; x + 1} ))
# insert_return(quote({ x = 2; return(x + 1)} ))
# insert_return(quote(while(TRUE) {  return(x + 1) }  ))
# insert_return(quote(while(TRUE) {  x + 1 }  ))
# insert_return(quote(if(x < 10) 20 else 40  ))
# insert_return(quote(if(x < 10) { x= 3; sqrt(x) } else 40  ))
# insert_return(quote(if(x < 10) { x= 3; sqrt(x) } else { x = 100; sqrt(x)}  ))
#

#' @export
insert_return = function(node) {
  UseMethod("insert_return")
}

#' @export
`insert_return.Brace` = function(node) {
  # Insert Return for last statement if not already.
  len = length(node$body)
  ret = insert_return(node$body[[len]])

  if (is.list(ret)) {
    node$body = append(node$body[-len], ret)
    for (x in ret)
      x$parent = node

  } else {
    node$body[[len]] = ret
    ret$parent = node
  }

  node
}

#' @export
insert_return.Function = function(node) {
  node$body = insert_return(node$body)

  node
}

#' @export
insert_return.If = function(node) {
  node$true = insert_return(node$true)
  node$false = insert_return(node$false)

  node
}

#' @export
insert_return.While = function(node) {
  # Need to insert a return(NULL) on following line
  ans = list(
    node,
    Return$new(Null$new())
  )

  if (is(node$parent, "Brace"))
    return (ans)

  Brace$new(ans)
}

#' @export
insert_return.For = insert_return.While

#' @export
insert_return.Literal = function(node) {
  Return$new(node)
}

#' @export
insert_return.Symbol = insert_return.Literal

#' @export
insert_return.Call = insert_return.Literal

#' @export
insert_return.Assign = insert_return.Literal

#' @export
insert_return.NULL = function(node) {
  Return$new(Null$new())
}

#' @export
insert_return.Return = function(node) {
  node
}


isSelect =
    # checks if the body and alternative of an if() statement are single expressions.
    # Select corresponds to the LLVM concept of a Select, i.e.,  x ? a : b
function(call)
  length(call) == 4 && all(sapply(call[3:4], isSingleExpression))

isSingleExpression =
function(e)
{
  if(is.atomic(e))
      return(TRUE)

     # if the expression is return(expr)  then say no.
  if(is.call(e) && as.character(e[[1]]) == "return")
      return(FALSE)
  
  ( 
    (is(e, "{") && length(e) == 2 && ( is.call(k <- e[[2]]) ) ) ||
    (is.call(k <- e))
  ) &&
  !(class(k) %in% c("while", "for", "if", "=", "<-", "<<-", "{"))
}
