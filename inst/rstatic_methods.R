library(rstatic)

`[[.Call` =
function(x, i, ...)
{
    if(i == 1)
        return(x$fn)

    x$args[[i-1L]]
}

#XX Breaks the print.ASTNode() method.
# format.ASTNode has an expression: 
#     is_method = vapply(members, function(f) is.function(x[[f]]), TRUE)
# R6 should have a mechanism to get the names of the methods, not find them by looking for
# method objects.

`[[.For` =
function(x, i, ... )
{
    switch(i,
           as.name("for"),  #1
           x$ivar,
           x$iter,
           x$body)
}


`[[.If` =
function(x, i, ... )
{
    switch(i,
           as.name("if"),  #1
           x$condition,
           x$true,
           x$false)
}


# need to do for  While, Function, ...


setOldClass(c("Symbol",  "ASTNode", "R6"     ))
setAs("Symbol", "character", function(from) from$name)

setOldClass(c("Integer", "Literal", "ASTNode", "R6"     ))
setAs("Integer", "integer", function(from) from$value)

setOldClass(c("Numeric", "Literal", "ASTNode", "R6"     ))
setAs("Numeric", "numeric", function(from) from$value)

setOldClass(c("Logical", "Literal", "ASTNode", "R6"     ))
setAs("Logical", "logical", function(from) from$value)

setOldClass(c("Character", "Literal", "ASTNode", "R6"     ))


setAs("logical", "Logical", function(from) Logical$new(from))
setAs("logical", "ASTNode", function(from) Logical$new(from))

setAs("integer", "Integer", function(from) Integer$new(from))
setAs("integer", "ASTNode", function(from) Integer$new(from))

setAs("numeric", "Numeric", function(from) Numeric$new(from))
setAs("numeric", "ASTNode", function(from) Numeric$new(from))

setAs("character", "Character", function(from) Character$new(from))
setAs("character", "ASTNode", function(from) Character$new(from))

setAs("character", "Symbol", function(from) Symbol$new(from)) # note don't convert to Character first.

setAs("character", "ASTNode", function(from) Character$new(from)) # note don't convert to Character first.


# Complex.

setOldClass(c("Call", "Application", "ASTNode", "R6"))
setAs("Call", "call", function(from) to_r(from))

setAs("call", "ASTNode", function(from) to_ast(from))
setAs("call", "Call", function(from) to_ast(from))

setOldClass(c("Brace", "Container", "ASTNode", "R6"))
setAs("{", "ASTNode", function(from) to_ast(from))
setAs("{", "Brace", function(from) to_ast(from))

parent = function(x, ...) x$parent

"parent<-" = function(x, ..., value) { x$parent = value ; x}


"args<-" = function(x, ..., value) UseMethod("args<-")
"args<-.Call" = function(x, ..., value) { x$args = lapply(value, as, "ASTNode"); x}



Call =
function(fn, args)
{
    args = lapply(args, as, "ASTNode")
    fn = as(fn, "Symbol")
    rstatic::Call$new(fn, args = args)
}


For =
function(condition, body, var = identifyLoopVar(condition), parent = NULL)
{
  rstatic::For$new(as(var, "Symbol"), as(condition, "ASTNode"), as(body, "ASTNode"), parent = parent)
}

if(FALSE) {
   k =  Call$new(Symbol$new("bob"), args = list(Integer$new(1), Numeric$new(10.3)))    
   as(k[[1]], "character")


   Call("abc", list(1L, 2.3))

   For(quote(1:n), quote(x[i] - x[i-1]), "i")
   For(quote(1:n), quote({ x[i]; x[i-1]}), "i")       
}    


identifyLoopVar =
    # identify the loop variable by finding the code
    #
function(body)
{

}



