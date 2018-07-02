#' Compute Symbol Definition and Use Sets
#'
#' This function computes the symbol definition and use sets for a code object.
#'
#' @param node (ASTNode) The code object.
#' @param ... Further arguments passed to or from other methods.
#' @param by_block (logical) When TRUE, the sets are computed for each Block.
#' When FALSE, the sets are computed for each line in each block.
#'
#' @return A two-element list. The first element contains the def sets and the
#' second element contains the use sets.
#'
#' @export
def_use_sets =
function(node, ...)
{
  UseMethod("def_use_sets")
}

#' @export
def_use_sets.Function =
function(node, ...)
{
  # FIXME: Include parameters in defs for entry block?
  def_use_sets(node$body, ...)
}

# This can probably be generalized to apply to Braces and other Containers.
def_use_sets_by_elt =
function(node, ...)
{
  # FIXME: The initial sets do not get passed on from this function.
  initial = list(def = list(), use = list())
  for (i in seq_along(node$contents)) {
    elt = node$contents[[i]]

    c(def, use) := def_use_sets(elt, ...)
    initial[["def"]][[i]] = def
    initial[["use"]][[i]] = use
  }

  initial
}

#' @export
def_use_sets.BlockList = def_use_sets_by_elt

#' @export
def_use_sets.Block =
function(node, ..., by_block = FALSE)
{
  if (by_block) {
    initial = list(def = character(0), use = character(0))
    for (line in node$contents)
      initial = def_use_sets_line(line, initial, ...)

    return (initial)
  }

  def_use_sets_by_elt(node, ...)
}


def_use_sets_line =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...)
{
  UseMethod("def_use_sets_line")
}


# Defs --------------------
#' @export
def_use_sets_line.Assign =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...)
{
  # DEF: Append to the vector.
  initial[["def"]] = union(initial[["def"]], node$write$name)

  def_use_sets_line(node$read, initial, ..., top = FALSE)
}

#' @export
def_use_sets_line.Return = def_use_sets_line.Assign

#' @export
def_use_sets_line.For =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...)
{
  # DEF: Append to the vector.
  initial[["def"]] = union(initial[["def"]], node$variable$name)

  def_use_sets_line(node$iterator, initial, ..., top = FALSE)
}


# Uses --------------------
#' @export
def_use_sets_line.Symbol =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...
  , only_undefined_uses = FALSE)
{
  # USE: Append to the vector.
  use = node$name
  #  ( all uses             OR use isn't in defs            )
  if ( !only_undefined_uses || !(use %in% initial[["def"]]) )
    initial[["use"]] = union(initial[["use"]], node$name)

  initial
}


# Other --------------------

# FIXME: def_use_sets_line.Phi

#' @export
def_use_sets_line.Invocation =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...)
{
  for (arg in node$args) {
    initial = def_use_sets_line(arg, initial, ...)
  }

  initial
}

#' @export
def_use_sets_line.Call =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...)
{
  def_use_sets_line(node$fn, NextMethod(), ...)
}

#' @export
def_use_sets_line.ConditionalBranch =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...)
{
  def_use_sets_line(node$condition, initial, ...)
}

#' @export
def_use_sets_line.Literal =
function(node
  , initial = list(def = character(0), use = character(0))
  , ...)
{
  initial
}

#' @export
def_use_sets_line.Branch = def_use_sets_line.Literal

#' @export
def_use_sets_line.Function = def_use_sets_line.Literal
