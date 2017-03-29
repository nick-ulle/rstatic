
<!-- README.md is generated from README.Rmd. Please edit that file -->
rstatic
=======

A package of data structures and tools for manipulating R code.

GNU R has built-in support for treating code as data, referred to variously as *programming on the language*, *metaprogramming*, or *reflection*. The `quote()` function (among others) returns the unevaluated parse tree for an R expression. A parse tree represents expressions as a tree where each node is an operation or an object. Function calls, primitive operations, and keywords are operations; variables and literal values are objects. The children of an operation are its arguments. Objects only appear as leaves in the tree, since they cannot have arguments. Parse trees can be manipulated using standard R syntax; for example, nodes can be accessed with `[[`, the indexing operator.

Several properties of R's parse trees make them an inconvenient data structure for modifying code:

1.  They use copy-on-write semantics, so nodes can't be referenced. Instead, each node stores all of its descendants, but is unaware of its ancestors. This also makes it difficult to track a specific node across a sequence of transformations of the tree.

2.  `call` nodes are used for several semantically distinct operations. For example, `repeat` and `return()` are represented by `call` objects. This devalues method dispatch as a strategy for tree traversal.

3.  They explicitly model the code's syntax, but syntax is rarely informative for translation, optimization, and other goals of code analysis.

This package contains alternatives to R's parse trees.

The `to_ast()` function converts an R expression to an abstract syntax tree (AST). The nodes of the AST are [R6](https://github.com/wch/R6/) classes, so they use reference semantics and every node is aware of its ancestors. Different classes are used for nodes that represent semantically distinct operations. An AST can be converted back to an R parse tree with the `to_r()` function.

### Installation

The easiest way to install this package is with devtools:

``` r
install.packages("devtools")

devtools::install_github("nick-ulle/rstatic")
```

### Usage

The function `to_ast` converts functions or quoted R code to a tree of ASTNode objects. For example:

``` r
library("rstatic")

f = function(x = 1, y = 1) {
  return (x^2 + y^2)
}

f_ast = to_ast(f)
print(f_ast)
## <Function> $body $clone $copy $initialize $params $parent $set_body $set_params
## function(x = 1, y = 1) {
##     return(x^2 + y^2)
## }
```

The output shows that the root node has class `Function`, which has several fields, and also shows the corresponding R code. The semantic components of the function are accessible through the fields. For instance, to get the parameters:

``` r
f_ast$params
## $x
## <Parameter> $base $clone $copy $default $initialize $n $name $parent $set_default
## pairlist(x = 1) 
## 
## 
## $y
## <Parameter> $base $clone $copy $default $initialize $n $name $parent $set_default
## pairlist(y = 1)
```

In interactive use, the `to_astq` function, which automatically quotes its argument, may be more convenient. For example:

``` r
ast = to_astq(mean(c(4, 2, NA), na.rm = TRUE))

print(ast)
## <Call> $args $clone $copy $fn $initialize $parent $set_args $set_fn
## mean(c(4, 2, NA), na.rm = TRUE)

ast$args
## [[1]]
## <Call> $args $clone $copy $fn $initialize $parent $set_args $set_fn
## c(4, 2, NA) 
## 
## 
## $na.rm
## <Logical> $clone $copy $initialize $parent $value
## TRUE
```

Note that the nodes of the tree have reference semantics, so extracting and modifying a node will also modify that node in the tree. This behavior is different from most R objects.

The function `to_r` converts a tree of ASTNodes back to R code:

``` r
to_r(ast)
## mean(c(4, 2, NA), na.rm = TRUE)
```

This works even if the tree has been modified, as long as the nodes still represent valid R code.

### Known Issues

See the [to-do list](TODO.md).

Additional Notes
----------------

Many analyses produce information which applies to specific nodes in the AST of a source program. For languages that do not enforce variable types (such as R), type inference is an example of this. Type inference associates a type with each variable, but the type of a variable may change over the course of the program. A useful type inference system must not only determine the set of types associated with a variable, but also map those types to specific occurrences of the variable. In other words, the information must be mapped back to the nodes in the AST. GNU R represents nodes by language objects, and there are three natural ways to map information to nodes:

1.  Assign a unique identifier to each language object.
2.  Set an attribute on each language object.
3.  Use an alternate representation for each node.

The first approach has each information object store a list of identifiers for associated nodes. The nodes can be looked up efficiently via a map from identifiers to locations in the AST. This extra layer of indirection is only necessary if the AST will be modified subsequent to analysis, which is typically the case. After modification, the map is updated to reflect the new locations of nodes so that the identifiers themselves do not need to be updated. Thus this approach is an ad-hoc reference system, which suggests it would be prudent to use R's existing facilities for reference semantics.

In the second approach, each node stores the associated information objects as attributes. Although simple, this has an important limitation compared to the third approach, which is discussed below.

The third approach converts the nodes from language objects to an alternate representation before analysis. The alternate representation stores both the original meaning of the node and the information collected by analyses. This is effectively the same as the second approach, with one exception: the alternate representation offers greater flexibility in traversing the AST. Specifically, a finer set of classes can be used for the alternate representation than those used for language objects. For example, GNU R represents both function calls and return statements as `call` objects even though they have very different semantic meaning. The alternate representation can use a separate class for each, which is convenient when traversing the AST by dynamic dispatch. Moreover, if the alternate representation has reference semantics, each node can store a reference to its parent node.

Based on these ideas, this package uses subclasses of the R6 class ASTNode to represent nodes.

\[TODO: Add more details about the ASTNode class and its subclasses.\]
