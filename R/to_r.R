#' Convert ASTNode to R Code
#'
#' This function convert an abstract syntax tree to the corresponding R code.
#' 
#' @param node (ASTNode) The tree to be converted.
#' @param use_ssa (logical) Use SSA numbering?
#' @param use_phi (logical) Include phi-functions?
#' @param use_inner_blocks (logical) Include inner blocks?
#'
#' @export
to_r =
function(node, ...) {
  UseMethod("to_r")
}

#' @export
to_r.BlockList =
function(node, ...) {
  body = lapply(node$body, function(block) {
    block = to_r(block, ...)
    as.list(block[-1])
  })
  body = unlist(body, recursive = FALSE)

  as.call(c(as.name("{"), body))
}

#' @export
to_r.Brace =
function(node, ...) {
  use_phi = list(...)[["use_phi"]]
  if (is.null(use_phi) || use_phi)
    phi = lapply(node$phi, to_r, ...)
  else
    phi = list()

  x = c(as.name("{"), phi, lapply(node$body, to_r, ...))
  as.call(x)
}


#' @export
to_r.Parenthesis =
function(node, ...) {
  call("(", to_r(node$args[[1]], ...))
}


#' @export
to_r.Next =
function(node, ...) {
  call("next")
}


#' @export
to_r.Break =
function(node, ...) {
  call("break")
}


#' @export
to_r.If =
function(node, ...) {
  use_inner_blocks = list(...)[["use_inner_blocks"]]
  if (!is.null(use_inner_blocks) && !use_inner_blocks)
    blocks = as.name("..")
  else if (is.null(node$false))
    blocks = to_r(node$true, ...)
  else
    blocks = c(to_r(node$true, ...), to_r(node$false, ...))

  as.call(c(as.name("if"), to_r(node$condition, ...), blocks))
}


#' @export
to_r.For =
function(node, ...) {
  use_inner_blocks = list(...)[["use_inner_blocks"]]
  if (!is.null(use_inner_blocks) && !use_inner_blocks)
    block = as.name("..")
  else
    block = to_r(node$body, ...)

  call("for", to_r(node$ivar, ...), to_r(node$iter, ...), block)
}


#' @export
to_r.While =
function(node, ...) {
  use_inner_blocks = list(...)[["use_inner_blocks"]]
  if (!is.null(use_inner_blocks) && !use_inner_blocks)
    block = as.name("..")
  else
    block = to_r(node$body, ...)

  if (node$is_repeat)
    call("repeat", block)
  else
    call("while", to_r(node$condition, ...), block)
}


#' @export
to_r.Assign =
function(node, ...) {
  call("=", to_r(node$write, ...), to_r(node$read, ...))
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

  use_ssa = list(...)[["use_ssa"]]
  if (is.null(use_ssa) || use_ssa)
    name = node$name
  else
    name = node$basename

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
