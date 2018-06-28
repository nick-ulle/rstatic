
# TODO

Built-ins where `to_ast()` fails:

*   [?] `Adobe_glyphs` -- This is not a function; unclear how/what rstatic
    should do with this or how it relates to code analysis.
*   [?] `tools::charset_to_Unicode` -- This is not a function.
*   [ ] package:stats - selfStart objects
    ```
    [1] "SSasymp"     "SSasympOff"  "SSasympOrig" "SSbiexp"    
    [5] "SSfol"       "SSfpl"       "SSgompertz"  "SSlogis"    
    [9] "SSmicmen"    "SSweibull"  
    ```
    These are functions that have unusual classes.

*   [ ] Warnings about `formals(args(fn))` on primitives that do not have named
    parameters.


* Code Normalization
  * [ ] pull out if-statements nested in other expressions
  * [ ] pull out loops nested in other expressions
  * [ ] determine evaluation order of default arguments
  * [x] collapse namespaces onto symbols
  * [x] `repeat` is `while (TRUE)`
  * [x] replacement functions: parse trees represent `length(x) <- 6` as
  `"<-"(length(x), 6)` but the meaning is `"length<-"(x, 6)`. That is, this is
  a call to a replacement function, not an assignment to a call.
  * [x] explicit `return()` at end of function
  * [x] *non-breaking:* dead code after `return()`, `next`, `break`

* Code Analysis
  * [ ] Collect reads correctly for replacement functions, function
    definitions, and any other missing cases. These need unit tests!
  * [ ] Collect global uses by called functions. Currently `toSSA()` only
    detects global uses in the function being converted to SSA form (and not in
    the functions that function calls).

* Syntax Objects (ASTNode subclasses)
  * [ ] `...`
  * [ ] S4
  * [ ] Closures (Functions must to keep track of parent environment)
  * [x] `<<-`
  * [x] `(`
  * [x] `break` and `next`
  * [x] `invisible()`
  * [x] Functions
  * [x] `[` and `[[`
  * [x] Namespace operators `::` and `:::`?
