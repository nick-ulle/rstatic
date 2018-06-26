+ match.call() equivalent?
  + substitute()

+ to_r may not be a good name since the rstatic classes are already R objects.

+ It is tedious and error prone to have to do mental mapping from R language objects
  to the R6 classes.  Provide both interfaces, e.g.,  For$ivar and For[[2]]
   +  So [[, ... for the same code to work on R AST and rstatic objects, e.g.
      call[[1]] == call$fn
   + See rstatic_methods.R

+ as.character and as(, "character") and other methods should be present.
  + See rstatic_methods.R

+ as() methods for going from R types to rstatic

+ If not already done, make interface non-R6 and much more convenient
  Instead of
```  
 k =  Call$new(Symbol$new("bob"), args = list(Integer$new(1), Numeric$new(10.3)))
```
 also have
``` 
 Call("bob", list(1, 10.3))
 Call("bob", , 1, 10.3)
```
This is a little tricky as Call is defined and we need to invoke Call$new.
   + Issue with overloading Call, Integer, ... as regular functions as then can't see  the Call R6 object.
     Want to avoid Call$new()  but instead use Call() function directly.

+ How well has R6 served you? What about reference classes?  S4?

+ Is there a way to get the names of the functions/methods in an R6 object without using [[
  See format.ASTNode where I change the class.
  Currently, I unclass the object to avoid the [[ method for the ASTNodes.

+ What is the increment in the For object?


+ Your AST traverse versus mine.  What's the conceptual difference and why couldn't you have adapted
  mine for your purposes?  Which came first?
