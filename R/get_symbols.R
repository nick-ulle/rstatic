#' Symbols Used by an Expression
#'
#' This function returns a character vector of the symbols used by an
#' expression.
#'
#' If you need both the symbols defined and the symbols used by an expression,
#' it is more efficient to first call `get_symbols()` on the expression and
#' then call `names_defined()` and/or `names_used()` on the result.
#'
#' @param x A language object, `ASTNode` object, or `SymbolSets` object.
#' @param ... Further arguments to methods.
#'
#' @return A character vector.
#' @examples
#' expr = quote(x = x + y)
#' names_used(expr)
#'
#' # If defined symbols and used symbols are both needed:
#' symbols = get_symbols(expr)
#' names_defined(symbols)
#' names_used(symbols)
#' @seealso [names_defined()], [get_symbols()], [CodeDepends::getInputs()]
#' @export
names_used =
function(x, ...)
{
  UseMethod("names_used")
}

#' @export
names_used.SymbolSets =
function(x, ...)
{
  used = vapply(x[["used"]], toString, NA_character_, ...)

  unique(used)
}

#' @export
names_used.default =
function(x, ...)
{
  sets = get_symbols(x, ...)
  names_used.SymbolSets(sets, ...)
}



#' Symbols Defined by an Expression
#'
#' This function returns a character vector of the symbols defined by an
#' expression.
#'
#' If you need both the symbols defined and the symbols used by an expression,
#' it is more efficient to first call `get_symbols()` on the expression and
#' then call `names_defined()` and `names_used()` on the result.
#'
#' @param x A language object, `ASTNode` object, or `SymbolSets` object.
#' @param ... Further arguments to methods.
#'
#' @return A character vector.
#' @examples
#' expr = quote(x = x + y)
#' names_defined(expr)
#'
#' # If defined symbols and used symbols are both needed:
#' symbols = get_symbols(expr)
#' names_defined(symbols)
#' names_used(symbols)
#' @seealso [names_used()], [get_symbols()], [CodeDepends::getInputs()]
#' @export
names_defined =
function(x, ...)
{
  UseMethod("names_defined")
}

#' @export
names_defined.SymbolSets =
function(x, ...)
{
  defined = vapply(x[["defined"]], function(d) {
    toString(get_first_defined(d), ...)
  }, NA_character_)

  unique(defined)
}

#' @export
names_defined.default =
function(x, ...)
{
  sets = get_symbols(x, ...)
  names_defined.SymbolSets(sets, ...)
}



SymbolSets =
function(defined = list(), used = list())
{
  structure(list(defined = defined, used = used), class = "SymbolSets")
}


# TODO: Eventually separate out called names from used names.

#' Get Sets of Defined and Used Symbols
#'
#' This function returns the set of symbols defined by an expression and the
#' set of symbols used by an expression.
#'
#' @param node (ASTNode) The expression to analyze.
#' @param initial (SymbolSets) The initial sets of defined/used expressions.
#' @param ... Further arguments to methods.
#' @param recursive (logical) Search for symbols in function definitions?
#' @param include_default_arguments (logical) Search for symbols in default
#' arguments?
#' @param only_undefined_uses (logical) Exclude used symbols that are defined
#' in `node` before they are used?
#'
#' @return A `SymbolSets` object, with elements `defined` and `used`. Element
#' `defined` is a list of expressions that define a symbol. Element `used` is a
#' list of used symbols.
#' @examples
#' ast = quote_ast(x <- sum(x, 1, y))
#' get_symbols(ast)
#' @seealso [names_defined()], [names_used()], [CodeDepends::getInputs()]
#' @export
get_symbols =
function(node, initial = SymbolSets(), ...)
{
  UseMethod("get_symbols")
}


# Defined symbols ----------------------------------------
#' @export
get_symbols.Assignment =
function(node, initial = SymbolSets(), ...)
{
  # DEF: Append to the vector.
  initial[["defined"]] = c(initial[["defined"]], node)

  get_symbols(node$read, initial, ...)
}


#' @export
get_symbols.Return = get_symbols.Assignment


#' @export
get_symbols.For =
function(node, initial = SymbolSets(), ...)
{
  # DEF: Append to the vector.
  initial[["defined"]] = c(initial[["defined"]], node)

  initial = get_symbols(node$iterator, initial, ...)

  NextMethod()
}


#' @export
get_symbols.Parameter =
function(node, initial = SymbolSets(), ..., include_default_arguments = FALSE)
{
  initial[["defined"]] = c(initial[["defined"]], node)

  if (include_default_arguments)
    get_symbols(node$default, initial, ...)
  else
    initial
}


# Used Symbols ----------------------------------------
#' @export
get_symbols.Symbol =
function(node, initial = SymbolSets(), ..., only_undefined_uses = FALSE)
{
  if (only_undefined_uses) {
    defined = lapply(initial[["defined"]], get_defines)
    if (node %in% defined)
      return (initial)
  }

  # USE: Append to the vector.
  initial[["used"]] = c(initial[["used"]], node)
  initial
}


#' @export
get_symbols.Function =
function(node, initial = SymbolSets(), ..., recursive = FALSE)
{
  # Don't recurse into the Function unless recursive = TRUE.
  if (recursive)
    NextMethod()
  else
    initial
}



#' @export
get_symbols.ASTNode =
function(node, initial = SymbolSets(), ...)
{
  for (child in children(node))
    initial = get_symbols(child, initial, ...)

  initial
}


#' @export
get_symbols.default =
function(node, initial = SymbolSets(), ...)
{
  get_symbols(to_ast(node), initial, ...)
}



# get_first_defined() ------------------------------
# This is a helper for names_defined().
get_first_defined =
function(node)
{
  UseMethod("get_first_defined")
}

#' @export
get_first_defined.Assignment =
function(node)
{
  node$write
}

#' @export
get_first_defined.Return = get_first_defined.Assignment

#' @export
get_first_defined.For =
function(node)
{
  node$variable
}

#' @export
get_first_defined.Parameter =
function(node)
{
  node$value
}

#' @export
get_first_defined.ASTNode =
function(node)
{
  character(0)
}
