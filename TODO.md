
# TODO

to_ast() can't handle  "Adobe_glyphs" (data.frame), 
"charset_to_Unicode" (noquote, hexmode) in package:tools
```
zz = lapply(ls("package:tools"), function(x) try(to_ast(get(x, "package:tools"))))
ls("package:tools")[(sapply(zz, is, 'try-error'))]
```
For package:base, "R.home"    "Vectorize"  (simple.list)

package:stats - selfStart objects
 [1] "SSasymp"     "SSasympOff"  "SSasympOrig" "SSbiexp"    
 [5] "SSfol"       "SSfpl"       "SSgompertz"  "SSlogis"    
 [9] "SSmicmen"    "SSweibull"  

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
