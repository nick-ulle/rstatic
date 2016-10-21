
# TODO

* [ ] Control flow
  * [x] if-statements
  * [x] while-loops
  * [ ] for-loops
  * [ ] `break` and `next`: keep list of broken nodes
  * [ ] `return()`
  * [ ] Separate classes for loop header blocks?
* [x] Population of parent field.
* [ ] Mutators are represented as assignment with a call as the write operand,
  but should be represented as a call to the mutator.
* [ ] Classes to represent:
    * [x] `break` and `next`
    * [ ] Lists
    * [ ] Data Frames
    * [x] `(`
    * [ ] S4
    * [ ] `...`
    * [ ] Invisible
    * [ ] Functions/Closures (anonymous functions need to be carried around)
* [ ] Do we support namespace operators `::` and `:::`?
* [ ] Unit tests.
* [ ] Expansion of the data structure discussion in the README.
* [x] ? Removal of `type` field from literals (other packages can map these).
* [ ] Bugfix: single-branch if-statements should not have an `else NULL`
  inserted when translated back to R


