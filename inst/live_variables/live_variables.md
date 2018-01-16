
_2018.01.15_

The goal is to compute a live variables analysis for a program.

A live variables analysis determines which variable definitions are live (or
conversely, dead) at the exit of each block. A variable definition is live if
it will be used in some later block. Thus redefining a variable kills the most
recent definition, while using a variable marks the most recent definition as
live.

For a simple example, consider the program
```{r}
x = 3
x = 4
y = x
```
In this example, we treat each line as a separate block.

In line 1, the kill variables are $\{x\}$ and the used variables are
$\emptyset$. Line 2 is the same. In line 3, the kill variables are $\{y\}$ and
the used variables are $\{x\}$.

With this information, we can compute the live variables. Initially, we set the
live variables for every block at entry and exit to $\emptyset$. Then we have

Line 1 entry: $\emptyset$
Line 1 exit: $\emptyset$
Line 2 entry: $\emptyset$
Line 2 exit: $\emptyset$
Line 3 entry: $\{x\}$
Line 3 exit: $\emptyset$

On the second iteration, we have

Line 1 entry: $\emptyset$
Line 1 exit: $\emptyset$
Line 2 entry: $\emptyset$
Line 2 exit: $\{x\}$
Line 3 entry: $\{x\}$
Line 3 exit: $\emptyset$

On the third iteration, we have

Line 1 entry: $\emptyset$
Line 1 exit: $\emptyset$
Line 2 entry: $\emptyset$
Line 2 exit: $\{x\}$
Line 3 entry: $\{x\}$
Line 3 exit: $\emptyset$

Since the sets do not change on this iteration, the analysis is complete. The
analysis shows that $x$ is live between the definition on line 2 and the use on
line 3.

In order to implement this, we need `killed()` and `used()` functions to
compute the killed and used variables for each block. Since killed and used
variables never change, the sets should either be precomputed or memoized.

We also need a data structure that can store the live variable sets at the
entry and exit of each block. In this case, it is convenient to use a list,
although that is not necessarily the most efficient data structure.

If we have a block with some kills and some uses, what is the right thing to
do? Suppose that the code

```{r}
x = 3
y = x
```

appears within one block. Then what are the killed and used sets? In this case,
$x$ is killed before it is used, so the killed set is $\{x\}$ and the used set
is $\emptyset$. Thus any use __after__ the first kill in the block does not
count as a use. As a sanity check, if the next block is

```{r}
z = x
```

then the killed set is $\{z\}$ used set is $\{x\}$. The used set will propagate
backward from the entry of this block to the exit of the previous block. It
does not propagate any further back (even if there were more blocks) because
$\{x\}$ is in the killed set of the previous block.
