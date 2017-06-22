
# rstatic

__rstatic__ is an R package for low-level static analysis of R code.

__rstatic__ represents R programs as control flow graphs (CFGs) and provides
extensible functions for analyzing them. CFGs are especially convenient for
implementing

1.  translation to languages with fewer hardware abstractions (for example,
    assembly languages), and

2.  analyses and optimizations from computer science literature.

This is because CFGs make potential paths of execution explicit and can model
any kind of control flow. By default, the package uses CFGs in static
single-assignment (SSA) form. SSA form provides complementary information about
data flow in a program by giving each variable (re)definition a unique name.

The __codetools__, __CodeDepends__, and __lintr__ packages extract similar
information from R programs, but they focus on presenting the information to
programmers, transforming R code, and generating new R code.


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
