
# TODO

* Refactor
  + [ ] Separate `$get_postorder()` from CFGraph class and add more traversal
  options
  * [ ] Return class can only have one arg

* Code Normalization
  * [ ] pull out if-statements nested in other expressions
  * [ ] pull out loops nested in other expressions
  * [x] `repeat` is `while (TRUE)`
  * [x] replacement functions: parse trees represent `length(x) <- 6` as
  `"<-"(length(x), 6)` but the meaning is `"length<-"(x, 6)`. That is, this is
  a call to a replacement function, not an assignment to a call.
  * [x] explicit `return()` at end of function
  * [ ] *non-breaking:* dead code after `return()`, `next`, `break`

* Control Flow
  * [ ] Collect reads correctly for replacement functions, function
    definitions, and any other missing cases. These need unit tests!
  * [x] if-statements
  * [x] while-loops
  * [x] for-loops
  * [x] `break` and `next`: keep list of broken nodes
  * [x] `return()`
  * [x] Static single-assignment
  * [x] Treat function names as variables for SSA
  * [x] Generate unique names for the for-loop helper variable `._iter_`.
    Nested for-loops will not advance correctly as long as the name is not
    unique.

* Syntax Objects (ASTNode subclasses)
  * [x] `(`
  * [x] `break` and `next`
  * [x] `invisible()`
  * [x] Functions
  * [ ] `[` and `[[`
  * [ ] `<<-`
  * [ ] `...`
  * [ ] Lists, Data Frames
  * [ ] S4
  * [ ] Closures (Functions must to keep track of parent environment)
  * [ ] Namespace operators `::` and `:::`?

* [ ] Unit Tests
* [ ] Expanded data structure discussion in the README.
* [x] Bugfix: single-branch if-statements should not have an `else NULL`
  inserted when translated back to R


