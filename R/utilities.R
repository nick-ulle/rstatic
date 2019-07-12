#' Create Error String from Object Class
#'
#' This function creates an error string that includes the class of an object.
#'
class_error_string =
function(object, msg = "Invalid class '%s'.", all_classes = FALSE)
{
  cl = class(object)
  if (all_classes)
    cl = toString(cl)
  else
    cl = cl[1L]

  sprintf(msg, cl)
}


#' Check Whether Named Member of R6 Object is Method
#'
#' This function checks whether the named members of an R6 object are methods
#' (as opposed to fields or active bindings).
#'
is_r6_method = function(name, obj) {
  vapply(name, function(f) {
    # Active bindings are not methods.
    !bindingIsActive(f, obj) && is.function(.subset2(obj, f))
  }, NA)
}


#' Collect Dots, Preserving Missing Arguments
#'
#' This function collects the `...` argument into a list. Missing arguments are
#' preserved as S3 objects of class `missing`.
#'
#' @seealso [rlang::dots_list()]
#'
list_dots_safely =
function(...)
{
  if (...length() == 1 && is.list(..1))
    return (..1)

  # It is possible to capture *unevaluated* dots:
  #
  #   dots = substitute(...())
  #
  # For the origin and more information about this trick, see:
  #
  #   Peter Meilstrup. 2018. substitute() on arguments in ellipsis ("dot dot
  #   dot")?. R Devel (August 2018).
  #
  #   https://adv-r.hadley.nz/quasiquotation.html#fnref68
  #
  # We want to evaluate the non-missing arguments. Since `...` may have been
  # passed down many calls, keeping track of the correct environment for
  # `eval()` is difficult. Instead, we let rlang do this for us.
  #
  dots = rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)

  # Replace the missing arguments with S3 objects.
  lapply(dots, function(dot) {
    if (rlang::is_missing(dot))
      structure(list(), class = "missing")
    else
      dot
  })
}


#' Unpack Elements
#'
#' This operator unpacks the elements of a vector or list into variables. Since
#' many other packages provide their own definition of the `:=` operator, this
#' function is not exported and only meant for internal use.
#'
`:=` =
function(x, y)
{
  x = substitute(x)
  if (length(x) < 2 || x[[1]] != "c")
    stop("left-hand side of unpack must be c(...).")
  x = x[-1]

  is_name = vapply(x, is.name, TRUE)
  if (!all(is_name))
    stop("cannot unpack into non-variables.")
  x = as.character(x)

  len_x = length(x)
  len_y = length(y)
  if (len_x != len_y)
    stop(sprintf("cannot unpack %i elements into %i variables.", len_y, len_x))

  keep = x != ""
  mapply(assign, x[keep], y[keep], MoreArgs = list(envir = parent.frame()),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  invisible(NULL)
}
