#' Get Object Argument from Call
#'
#' This function gets the object argument from calls that access slots, subset,
#' or replace an object.
#'
#' @param node A call with arguments.
#'
#' @return The object argument from the call.
#'
#' @export
arg_object =
function(node)
{
  UseMethod("arg_object")
}


#' @export
arg_object.Subset =
function(node)
{
  node$args$contents[[1L]]
}


#' @export
arg_object.Replacement =
function(node)
{
  node$read$args$contents[[1L]]
}


#' @export
arg_object.ASTNode =
function(node)
{
  msg = class_error_string(node,
    "Cannot get object argument for object of class '%s'.")
  stop(msg)
}


#' @export
arg_object.default =
function(node)
{
  arg_object(to_ast(node))
}



# arg_index ------------------------------------------------------------

#' Get Index Arguments from Call
#'
#' This function gets the index arguments from calls that access slots, subset,
#' or replace an object.
#'
#' @param node A call with arguments.
#'
#' @return A list of index arguments.
#'
#' @export
arg_index =
function(node)
{
  UseMethod("arg_index")
}


#' @export
arg_index.Subset1 =
function(node)
{
  # First argument is always the object.
  args = node$args$contents[-1L]

  # If last argument is called "drop", remove it as well.
  idx = match("drop", names(args), 0L)
  if (idx != 0L)
    args = args[-idx]

  args
}


#' @export
arg_index.Subset2 =
function(node)
{
  # First argument is always the object.
  args = node$args$contents[-1L]

  # If last argument is called "exact", remove it as well.
  idx = match("exact", names(args), 0L)
  if (idx != 0L)
    args = args[-idx]

  args
}


#' @export
arg_index.SubsetDollar =
function(node)
{
  # First argument is always the object.
  args = node$args$contents[-1L]

  args
}


#' @export
arg_index.Replacement1 =
function(node)
{
  # First argument is always the object.
  args = node$read$args$contents[-1L]

  # Last argument is always the value.
  args = args[-length(args)]

  args
}


#' @export
arg_index.Replacement2 = arg_index.Replacement1


#' @export
arg_index.ReplacementDollar = arg_index.Replacement1


#' @export
arg_index.ASTNode =
function(node)
{
  msg = class_error_string(node,
    "Cannot get index arguments for object of class '%s'.")
  stop(msg)
}


#' @export
arg_index.default =
function(node)
{
  arg_index(to_ast(node))
}


#' @export
get_index =
function(node)
{
  .Deprecated("arg_index")
  arg_index(node)
}



# arg_value ------------------------------------------------------------

#' Get Value Argument from Replacement Call
#'
#' This function gets the value argument from calls that replace an object.
#'
#' @param node (Replacement) A call to a replacement function.
#'
#' @return The `value` argument from the call.
#'
#' @export
arg_value =
function(node)
{
  UseMethod("arg_value")
}


#' @export
arg_value.Replacement =
function(node)
{
  args = node$read$args$contents
  args[[length(args)]]
}


#' @export
arg_value.ASTNode =
function(node)
{
  msg = class_error_string(node,
    "Cannot get value argument for object of class '%s'.")
  stop(msg)
}


#' @export
arg_value.default =
function(node)
{
  arg_value(to_ast(node))
}
