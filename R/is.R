#' Is Object a Symbol?
#'
#' This function checks whether the `x` argument is an rstatic Symbol. The
#' optional second argument can be used to check for a specific name.
#'
#' @param x The object to check.
#' @param name (character) A name to compare against, or `NA` to skip checking
#' the name (the default).
#' @return A logical value: is `x` a Symbol, and does it match the `name`
#' argument?
#'
#' @examples
#' x = quote_ast(hi)
#' is_symbol(x) # TRUE
#' is_symbol(x, "hi") # TRUE
#' is_symbol(x, "hello") # FALSE
#' @export
is_symbol =
function(x, name = NA_character_, ssa = TRUE)
{
  if (is(x, "Symbol")) {
    if (is.na(name))
      return (TRUE)
    else if (ssa)
      return (x$ssa_name == name)
    else
      return (x$basename == name)
  }

  FALSE
}
