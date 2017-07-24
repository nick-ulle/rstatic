
#' @export
astTraverse =
function(ast, fun, ...)
{
  fun(ast, ...)
  invisible(UseMethod("astTraverse"))
}

#' @export
astTraverse.Function =
function(ast, fun, ...)
{
   astTraverse(ast$params, fun, ...)
   invisible(astTraverse(ast$body, fun, ...))
}

#' @export
astTraverse.Assign =
function(ast, fun, ...)
{
   astTraverse(ast$read, fun, ...)
   astTraverse(ast$write, fun, ...)   
}

#' @export
astTraverse.Call =
function(ast, fun, ...)
{
  astTraverse(ast$fn, fun, ...)
  astTraverse(ast$args, fun, ...)  
}

#' @export
astTraverse.Brace =
function(ast, fun, ...)
{
   astTraverse(ast$body, fun, ...)
}

#' @export
astTraverse.Symbol =
function(ast, fun, ...)
{
    fun(ast)
}

#' @export
astTraverse.Literal = astTraverse.Symbol

#' @export
astTraverse.For =
function(ast, fun, ...)
{
   astTraverse(ast$ivar, fun, ...)    
   astTraverse(ast$iter, fun, ...)
   astTraverse(ast$body, fun, ...)   
}

#' @export
astTraverse.Return =
function(ast, fun, ...)
{
  astTraverse(ast$args, fun, ...)
}

#' @export
astTraverse.list =
function(ast, fun, ...)
  invisible(lapply(ast, astTraverse, fun, ...)    )

#' @export
astTraverse.default =
function(ast, fun, ...)
{
  warning("no method for astTraverse for ",  class(ast)[1])
}

#' @export
astTraverse.While =
function(ast, fun, ...)
{
    astTraverse(ast$condition, fun, ...)
    astTraverse(ast$body, fun, ...)    
}


#' @export
astTraverse.If =
function(ast, fun, ...)
{
  astTraverse(ast$condition, fun, ...)
  astTraverse(ast$true, fun, ...)
  if(length(ast$false))
      astTraverse(ast$false, fun, ...)  
}

########################

#' @export
insertNode =
function(into, atNode, value, before = TRUE, field = getASTFieldName(into))
{
  vals = get(field, into)    
  w = sapply(vals, identical, atNode)

  if(!any(w))
     stop("atNode is not in the object")
  if(sum(w) > 1)
     stop("more than one matching node")

  vals =     if(before) {
                 if(which(w) == 1)
                    c(value, vals)
                 else {
                    tmp = split(into$body, cumsum(w))
                    c(tmp[[1]], value, tmp[[2]])
                 }
              } else {
                 vals = into$body
                 if(w[1])
                     c(vals[[1]], value, vals[-1])
                 else {
                     i = seq(1, length = which(w))
                     c(vals[i], value, vals[-i])
                 }
              }
   assign(field, vals, into)
}

getASTFieldName =
function(into)
{
    switch(class(into)[1],
           Call = "args",
           Return = "args",
           "body")
}

#' @export
replaceNode =
    # Eventually we will use field to allow setting arbitrary fields
    # We'll use get(field, into) and look for set_ and get_ methods for that field.
function(into, node, value, multi = is.list(value),
         field = getASTFieldName(into),
         error = TRUE)
{
        # more general than body.    
  vals = get(field, into)
  if(!is.list(vals) && identical(node, vals)) {
     if(multi) {
       value = Brace$new(value)
       assign(field, value, into)
       return(NULL)
     }
     w = TRUE  
  } else {
    w = sapply(vals, identical, node)
  }
  
  if(!any(w)) {
      if(error)
          stop("node is not in the object")
      else  # XXX there are recursive situations where we change the node and then cannot find it in subsequent calls. See makeIntegerLiterals in R2llvm/R/rewrite.R
          return(NULL)
  }
  if(sum(w) != 1)
      stop("more than one matching node")

  if(multi) {
      if(length(w) == 1)
          nvals = value
      else {
          tmp = split(vals, cumsum(w))
          nvals = c(tmp[[1]],  value, tmp[[2]][-1])
      }
      sapply(value, function(x) x$parent = into)
  } else {
    vals[[which(w)]] = value
    nvals = vals
    value$parent = into
  }
  assign(field, nvals, into)

  NULL
}



