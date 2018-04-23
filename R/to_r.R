#' Convert ASTNode to R Code
#'
#' This function convert an abstract syntax tree to the corresponding R code.
#' 
#' @param node (ASTNode) The tree to be converted.
#' @param keep_ssa (logical) Use SSA numbering?
#' @param use_phi (logical) Include phi-functions?
#' @param use_inner_blocks (logical) Include inner blocks?
#'
#' @export
to_r =
function(node, ...) {
  UseMethod("to_r")
}


# ---

# FIXME:
to_r.Label =
function(node, ...) {
  as.name(node$name)
}

# FIXME:
to_r.Branch =
function(node, ...) {
  call("Branch", node$target)
}

#' @export
to_r.Brace =
function(node, ...) {
  #use_phi = list(...)[["use_phi"]]
  #if (is.null(use_phi) || use_phi)
  #  phi = lapply(node$phi, to_r, ...)
  #else
  #  phi = list()

  x = c(as.name("{"), lapply(node$body, to_r, ...))
  as.call(x)
}


#' @export
to_r.Parenthesis =
function(node, ...) {
  call("(", to_r(node$args[[1]], ...))
}


#' @export
to_r.Next =
function(node, ...) call("next")


#' @export
to_r.Break =
function(node, ...) call("break")


#' @export
to_r.If =
function(node, ...) {
  true = to_r(node$true, ...)
  false = to_r(node$false, ...)
  condition = to_r(node$condition, ...)

  call("if", condition, true, false)
}

#' @export
to_r.For = function(node, ...) {
  body = to_r(node$body, ...)
  variable = to_r.Symbol(node$variable, ...)
  iterator = to_r(node$iterator, ...)

  call("for", variable, iterator, body)
}


#' @export
to_r.While =
function(node, ...) {
  body = to_r.Brace(node$body)

  if (node$is_repeat) {
    call("repeat", body)
  } else {
    condition = to_r(node$condition, ...)
    call("while", condition, body)
  }
}


#' @export
to_r.Assign =
function(node, ...) {
  read = to_r(node$read, ...)
  write = to_r(node$write, ...)

  call("=", write, read)
}


#' @export
to_r.Call =
function(node, ...) {
  fn = to_r(node$fn, ...)
  args = lapply(node$args, to_r, ...)

  as.call(append(fn, args))
}

#' @export
to_r.Phi =
function(node, ...) {
  reads = lapply(node$read, to_r, ...)
  phi = as.call(append(as.name("Phi"), reads))
  call("=", to_r(node$write, ...), phi)
}

#' @export
to_r.Replacement =
function(node, ...) {
  lhs = to_r(node$read, ...)

  # Delete the <- in the function name.
  fn = as.character(lhs[[1]])
  fn = gsub("<-", "", fn, fixed = TRUE)
  lhs[[1]] = as.symbol(fn)

  # Set the write variable.
  lhs[[2]] = to_r(node$write, ...)

  len = length(lhs)

  call("=", lhs[-len], lhs[[len]])
}

#' @export
to_r.Return =
function(node, ...) {
  call("return", to_r(node$read, ...))
}

#' @export
to_r.Symbol =
function(node, ...) {
  # Handle empty arguments.
  if (node$basename == "")
    return (quote(expr = ))

  keep_ssa = list(...)[["keep_ssa"]]
  if (is.null(keep_ssa) || !keep_ssa)
    name = node$basename
  else
    name = node$name

  if (is.na(node$namespace))
    as.name(name)
  else
    call(node$namespace_fn$name, as.name(node$namespace), as.name(name))
}


#' @export
to_r.Parameter =
function(node, ...) {
  param =
    if (is.null(node$default))
      pairlist(quote(expr = ))
    else
      pairlist(to_r(node$default, ...))

  names(param) = node$name
  return (param)
}


#' @export
to_r.Function =
function(node, ...) {
  params = pairlist()
  for (param in node$params)
    params = append(params, to_r(param, ...))

  if (is.null(node$body))
    call("function", as.pairlist(params), as.symbol(".."))
  else
    call("function", as.pairlist(params), to_r(node$body, ...))
}


#' @export
to_r.Primitive =
function(node, ...) {
  .Primitive(node$fn$name)
}


#' @export
to_r.Literal =
function(node, ...) {
  node$value
}
