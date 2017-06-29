astTraverse =
function(ast, fun, ...)
{
  fun(ast)
  invisible(UseMethod("astTraverse"))
}

astTraverse.Function =
function(ast, fun, ...)
{
   astTraverse(ast$params, fun, ...)
   invisible(astTraverse(ast$body, fun, ...))
}

astTraverse.Assign =
function(ast, fun, ...)
{
   astTraverse(ast$read, fun, ...)
   astTraverse(ast$write, fun, ...)   
}

astTraverse.Call =
function(ast, fun, ...)
{
  astTraverse(ast$fn, fun, ...)
  astTraverse(ast$args, fun, ...)  
}

astTraverse.Brace =
function(ast, fun, ...)
{
   astTraverse(ast$body, fun, ...)
}

astTraverse.Symbol = astTraverse.Literal =
function(ast, fun, ...)
{
    fun(ast)
}

astTraverse.For =
function(ast, fun, ...)
{
   astTraverse(ast$ivar, fun, ...)    
   astTraverse(ast$iter, fun, ...)
   astTraverse(ast$body, fun, ...)   
}

astTraverse.Return =
function(ast, fun, ...)
{
  astTraverse(ast$args, fun, ...)
}

astTraverse.list =
function(ast, fun, ...)
  invisible(lapply(ast, astTraverse, fun, ...)    )

astTraverse.default =
function(ast, fun, ...)
{
  warning("no method for astTraverse for ",  class(ast)[1])
}

insertBefore =
function(into, beforeNode, value)
{

}
replaceNode =
    # Eventually we will use field to allow setting arbitrary fields
    # We'll use get(field, into) and look for set_ and get_ methods for that field.
function(into, node, value, multi = is.list(value), field = "body")
{
  w = sapply(into$body, identical, node) #XXX more general than body.
  if(!any(w))
      stop("node is not in the object")
  if(sum(w) != 1)
      stop("more than one matching node")

  if(multi) {
      tmp = split(into$body, cumsum(w))
      into$body = c(tmp[[1]],  value, tmp[[2]])
      sapply(value, function(x) x$parent = into)
  } else {
    into$body[[which(w)]] = value
    value$parent = into
  }

  NULL
}


rewriteFor =
function(node, ...)
{
  if(is(node, "For"))   {
      # Process the body first for nested loops.

     # cond = substitute(i < n, list(i = as.name(i)
     #XXX cover more situations of course e.g. n:2 and i >= 2,  1:length(x)
    cond = rstatic::Call$new("<", list(node$ivar$copy(), node$iter$args[[2]]$copy()))
       # might want to write Call(++, i) or Call(intIncr, i) so the compiler could recognize this.
    inc = rstatic::Call$new("+", list(node$ivar, rstatic::Integer$new(1L)))
    o = b = node$body$copy()

    if(!is(b, "Brace"))
        b = rstatic::Brace$new(list(b))

    b$body = append(b$body, inc)
    whileLoop = rstatic::While$new(cond, b)
    init = rstatic::Assign$new(node$ivar$copy(), node$iter$args[[1]]$copy())    
    replaceNode(node$parent, node, list(init, whileLoop))
   }
  
  TRUE
}

if(FALSE) {
f =
function(n)
{
  total = 0L
  for(i in 2L:n) # with and without { around body.
      total = total + i
  return(total)
}
ast = rstatic::to_ast(f)
z = astTraverse(ast, function(x) print(class(x)))
z = astTraverse(ast, rewriteFor)
ast
}

