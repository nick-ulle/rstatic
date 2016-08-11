
# ast

This package contains data structures and tools for manipulating R's abstract
syntax trees (ASTs).

### Usage 

See the `to_ast` function.


## The ASTNode Data Structure

Many analyses produce information which applies to specific nodes in the AST of
a source program. For languages that do not enforce variable types (such as R),
type inference is an example of this. Type inference associates a type with
each variable, but the type of a variable may change over the course of the
program. A useful type inference system must not only determine the set of
types associated with a variable, but also map those types to specific
occurrences of the variable. In other words, the information must be mapped
back to the nodes in the AST. GNU R represents nodes by language objects, and
there are three natural ways to map information to nodes:

1. Assign a unique identifier to each language object.
2. Set an attribute on each language object.
3. Use an alternate representation for each node.

The first approach has each information object store a list of identifiers for
associated nodes. The nodes can be looked up efficiently via a map from
identifiers to locations in the AST. This extra layer of indirection is only
necessary if the AST will be modified subsequent to analysis, which is
typically the case. After modification, the map is updated to reflect the new
locations of nodes so that the identifiers themselves do not need to be
updated. Thus this approach is an ad-hoc reference system, which suggests it
would be prudent to use R's existing facilities for reference semantics.

In the second approach, each node stores the associated information objects as
attributes. Although simple, this has an important limitation compared to the
third approach, which is discussed below.

The third approach converts the nodes from language objects to an alternate
representation before analysis. The alternate representation stores both the
original meaning of the node and the information collected by analyses. This is
effectively the same as the second approach, with one exception: the alternate
representation offers greater flexibility in traversing the AST. Specifically,
a finer set of classes can be used for the alternate representation than those
used for language objects. For example, GNU R represents both function calls
and return statements as `call` objects even though they have very different
semantic meaning. The alternate representation can use a separate class for
each, which is convenient when traversing the AST by dynamic dispatch.
Moreover, if the alternate representation has reference semantics, each node
can store a reference to its parent node.

Based on these ideas, this package uses subclasses of the R6 class ASTNode to
represent nodes.

[TODO: Add more details about the ASTNode class and its subclasses.]


## TODO / WIP Notes

### Parameter

What if the user supplies an argument whose type conflicts with the default?

Need a Parameter class because parameters are typed just like symbols.


### Function

Stores the args as a list of ASTNodes and also the defaults as a list of
ASTNodes. 

Also need a structure for storing a function; this is what `ast.function`
should return.

This is technically a kind of literal.
