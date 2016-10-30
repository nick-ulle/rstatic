
# TODO

* [ ] Code normalization
  * [x] `repeat` is `while (TRUE)`
  * [ ] if-statements nested in other expressions
  * [ ] loops nested in other expressions
  * [ ] mutators: parse trees represent `length(x) <- 6` as
  `"<-"(length(x), 6)` but the meaning is `"length<-"(x, 6)`. That is, this is
  a call to a mutator, not an assignment to a call.

* [ ] Control flow
  * [x] if-statements
  * [x] while-loops
  * [ ] for-loops
  * [x] `break` and `next`: keep list of broken nodes
  * [ ] `return()`
  * [x] Separate classes for loop header blocks?

* [ ] R6 classes for syntax
    * [x] `(`
    * [x] `break` and `next`
    * [x] `invisible()`
    * [ ] Functions/Closures (anonymous functions need to be carried around)
    * [ ] Lists
    * [ ] Data Frames
    * [ ] `...`
    * [ ] Namespace operators `::` and `:::`?
    * [ ] S4

* [x] Population of parent field.
* [ ] Unit tests.
* [ ] Expansion of the data structure discussion in the README.
* [x] ? Removal of `type` field from literals (other packages can map these).
* [x] Bugfix: single-branch if-statements should not have an `else NULL`
  inserted when translated back to R


