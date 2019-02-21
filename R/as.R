#' @export
as.call =
function(x)
{
  UseMethod("as.call")
}

#' @export
as.call.Call =
function(x)
{
  as_language.Call(x)
}

#' @export
as.call.default = base::as.call


# as names/symbols ----------------------------------------

#' @export
as.name =
function(x)
{
  UseMethod("as.name")
}

#' @export
as.name.Symbol =
function(x)
{
  base::as.name(x$value)
}

#' @export
as.name.default = base::as.name

#' @export
as.symbol =
function(x)
{
  UseMethod("as.symbol")
}

#' @export
as.symbol.Symbol =
function(x)
{
  base::as.symbol(x$value)
}

#' @export
as.symbol.default = base::as.symbol


# as other literals ----------------------------------------

#' @export
as.character.Symbol =
function(x, ...)
{
  x$value
}

#' @export
as.character.Character =
function(x, ...)
{
  x$value
}

#' @export
as.character.Literal =
function(x, ...)
{
  as.character(x$value, ...)
}


#' @export
as.complex.Complex =
function(x, ...)
{
  x$value
}


#' @export
as.complex.Literal =
function(x, ...)
{
  as.complex(x$value)
}


#' @export
as.double.Numeric =
function(x, ...)
{
  x$value
}

#' @export
as.double.Literal =
function(x, ...)
{
  as.double(x$value, ...)
}


#' @export
as.integer.Integer =
function(x, ...)
{
  x$value
}

#' @export
as.integer.Literal =
function(x, ...)
{
  as.integer(x$value, ...)
}


#' @export
as.logical.Logical =
function(x, ...)
{
  x$value
}

#' @export
as.logical.Literal =
function(x, ...)
{
  as.logical(x$value, ...)
}
