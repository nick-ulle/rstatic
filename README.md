# rstatic

__rstatic__ is a package that makes it easier to analyze R code. These are the
guiding principles:

*   Reference semantics make code easier to transform. Want to make major
    code transformations without losing track of an important expression?
    References have you covered. __rstatic__'s code objects have reference
    semantics by way of __[R6][]__.

*   Method dispatch on code objects makes recursive descent algorithms easier
    to understand. This is more effective if code is organized into meaningful,
    extensible classes. __rstatic__'s class hierarchy is arranged according to
    the semantics of R.

*   Access to the parent of a node in an abstract syntax tree is useful for
    some analyses. __rstatic__ transparently keeps track of each node's
    parents.

*   Access to code elements by name is clearer than by index. We'd rather write
    `my_call$args[[2]]` to access a call's second argument than `my_call[[3]]`.
    __rstatic__ uses a consistent set of names for code elements.

*   Abstract syntax trees are not ideal for analyses that need control- and
    data-flow information. __rstatic__ can convert code to a control flow graph
    in static single assignment (SSA) form. SSA form exposes data flows by
    giving each variable definition a unique name.

The __codetools__ and __[CodeDepends][]__ packages use R's built-in language
objects to extract similar information from code. They may be more appropriate
for quick, ad-hoc analyses.

[R6]: https://github.com/r-lib/R6
[CodeDepends]: https://github.com/duncantl/CodeDepends

## Installation

__rstatic__ is unstable and under active development, so it's not yet available
on CRAN. To install, open an R prompt and run:

```r
install.packages("devtools")

devtools::install_github("nick-ulle/rstatic")
```


## Usage

The package includes a vignette that serves as an introduction. To access the
vignette, open an R prompt and run:

```r
library(rstatic)
vignette("rstatic-intro")
```


## Known Issues

See the [to-do list](TODO.md).

<!--
## Discussion

GNU R has built-in support for treating code as data, referred to variously as
_programming on the language_, _metaprogramming_, or _reflection_. The
`quote()` function (among others) returns the unevaluated parse tree for an R
expression.

A _parse tree_ represents programs as a tree of operations and objects.
Function calls, primitive operations, and keywords are operations. Arguments to
an operation appear as its children in the tree. Variables and literal values
are objects. Objects only appear as leaves in the tree, since they cannot have
arguments.

Parse trees can be manipulated using standard R syntax. Nonetheless, parse
trees and R's specific implementation of them can be inconvenient for static
analysis:

1.  Syntax is represented, but only semantics are relevant. For example, every
    for-loop is semantically equivalent to a specific while-loop, but because
    they are syntactically distinct, parse trees give them different
    representations. This complicates analysis.

2.  Some operations with similar syntax but different semantics are implemented
    with the same class. For example, `repeat` and `return()` are both
    represented by `call` nodes. This makes method dispatch less effective as a
    tree traversal strategy.

3.  It's impossible to create a reference to a node because the implementation
    respects R's default copy-on-write behavior. Consequently, each node stores
    all of its descendants but is unaware of its ancestors. This also makes it
    difficult to track a specific node across a sequence of transformations of
    the tree.

On the other hand, a _control flow graph_ represents programs as a graph. Edges
are branches in the program, while nodes (or _basic blocks_) contain code that
executes without branching.
-->
