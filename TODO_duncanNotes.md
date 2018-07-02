+   match.call() equivalent?
    +   substitute()

+   It is tedious and error prone to have to do mental mapping from R language
    objects to the R6 classes.  Provide both interfaces, e.g.,  `For$ivar` and
    `For[[2]]`
    +   So `[[`, ... for the same code to work on R AST and rstatic objects,
        e.g. `call[[1]] == call$fn`
    +   See rstatic_methods.R

+   S4 `as(, "character")` and other methods should be present.
    +   See rstatic_methods.R

+   `as()` methods for going from R types to rstatic

+   If not already done, make interface non-R6 and much more convenient.
    Instead of
    ```r
    Call$new(Symbol$new("bob"), args = list(Integer$new(1), Numeric$new(10.3)))
    ```
    also have
    ```r
     Call("bob", list(1, 10.3))
     Call("bob", , 1, 10.3)
    ```
    This is a little tricky as Call is defined and we need to invoke `Call$new`

    +   Issue with overloading Call, Integer, ... as regular functions as then
        can't see  the Call R6 object. Want to avoid `Call$new()`  but instead
        use `Call()` function directly.

+   How well has R6 served you? What about reference classes?  S4?
