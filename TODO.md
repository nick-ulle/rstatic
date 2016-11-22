
# TODO

* [ ] Code normalization
  * [x] `repeat` is `while (TRUE)`
  * [ ] if-statements nested in other expressions
  * [ ] loops nested in other expressions
  * [x] replacement functions: parse trees represent `length(x) <- 6` as
  `"<-"(length(x), 6)` but the meaning is `"length<-"(x, 6)`. That is, this is
  a call to a replacement function, not an assignment to a call.
  * [ ] explicit `return()` at end of function
  * [ ] *non-breaking:* dead code after `return()`, `next`, `break`

* [x] Control flow
  * [x] if-statements
  * [x] while-loops
  * [x] for-loops
  * [x] `break` and `next`: keep list of broken nodes
  * [x] `return()`

* [ ] R6 classes for syntax
  * [x] `(`
  * [x] `break` and `next`
  * [x] `invisible()`
  * [x] Functions
  * [ ] `<<-`
  * [ ] `...`
  * [ ] Namespace operators `::` and `:::`?
  * [ ] Lists, Data Frames
  * [ ] S4

* [ ] Reduce number of unnecessary blocks in generated CFGs.
* [ ] Remove Brace class (just use lists)?
* [ ] Return class can only have one arg.
* [ ] Use ASTNode objects for call names, etc...
* [ ] Closures (Functions must to keep track of parent environment)
* [x] Population of parent field.
* [ ] Unit tests.
* [ ] Expansion of the data structure discussion in the README.
* [x] ? Removal of `type` field from literals (other packages can map these).
* [x] Bugfix: single-branch if-statements should not have an `else NULL`
  inserted when translated back to R


