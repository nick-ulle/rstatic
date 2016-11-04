#
# Methods for converting to trees of ASTNode objects.
#

#' @include ast_node.R
NULL


#' Convert R Code to ASTNodes
#'
#' This function converts R code to a tree of ASTNode objects.
#'
#' @param expr R code to be converted.
#' @export
to_ast = function(expr, parent = NULL) {
  to_ast_(substitute(expr), parent)
}


#' Convert Quoted R Code to ASTNodes
#'
#' This function converts quoted R code to a tree of ASTNode objects. In other
#' words, this is the standard evaluation version of to_ast().
#'
#' @param expr (language) quoted R code to be converted.
#' @export
to_ast_ = function(expr, parent = NULL) {
  UseMethod("to_ast_")
}


#to_ast_.function = function(expr) {
#
#}


#' @export
to_ast_.if    = function(expr, parent = NULL) {
  node = If$new(parent)
  node$predicate = to_ast_(expr[[2]], node)
  node$true  = to_ast_(expr[[3]], node)
  node$false = if (length(expr) == 4) to_ast_(expr[[4]], node)
               else NULL

  return (node)
}

#' @export
to_ast_.for = function(expr, parent = NULL) {
  node = For$new(parent)
  node$ivar = to_ast_(expr[[2]], node)
  node$iter = to_ast_(expr[[3]], node)
  node$body = to_ast_(expr[[4]], node)
  return (node)
}

#' @export
to_ast_.while = function(expr, parent = NULL) {
  node = While$new(parent)
  node$predicate = to_ast_(expr[[2]], node)
  node$body = to_ast_(expr[[3]], node)
  return (node)
}

#' Convert a repeat to an ASTNode
#'
to_ast_repeat = function(expr, parent = NULL) {
  node = While$new(parent, predicate = TRUE, is_repeat = TRUE)
  node$body = to_ast_(expr[[2]], node)
  return (node)
}


#' @export
`to_ast_.=` = function(expr, parent = NULL) {
  read = expr[[3]]
  write = expr[[2]]

  if (inherits(write, "call")) {
    node = Replacement$new(parent, paste0(write[[1]], "<-"))
    read = to_ast_(read, node)
    write = lapply(write[-1], to_ast_, node)
    # FIXME: the read argument is for the "value" parameter.
    node$args = append(write, read)

  } else {
    node = Assign$new(parent)
    node$read = to_ast_(read, node)
    node$write = to_ast_(write, node)
  }

  return (node)
}

#' @export
`to_ast_.<-` = `to_ast_.=`


#' @export
to_ast_.call = function(expr, parent = NULL) {
  name = as.character(expr[[1]])

  # Handle "calls" that don't use the standard call syntax. Most of these are
  # actually keywords.
  if (name == "function")
    return (to_ast_function_def(expr, parent))
  else if (name == "repeat")
    return (to_ast_repeat(expr, parent))
  else if (name == "break")
    return (Break$new(parent))
  else if (name == "next")
    return (Next$new(parent))
  
  # The standard call syntax applies, so construct an appropriate node.
  node = 
    if (name == "return") {
      Return$new(parent)
    } else if (name == "invisible") {
      Return$new(parent, is_invisible = TRUE)
    } else if (name == ".Internal") {
      # TODO: .C .Fortran .Call .External
      Internal$new(parent)
    } else {
      Call$new(parent, name)
    }

  node$args = lapply(expr[-1], to_ast_, node)
  return (node)
}

#' Convert a function definition to an ASTNode.
#'
to_ast_function_def = function(expr, parent = NULL) {
  # TODO: Assign scope to function.
  node = Function$new(parent)

  node$params = mapply(function(name, default) {
    param = Parameter$new(parent, name)

    param$default =
      if (class(default) == "name" && default == "")
        NULL
      else
        to_ast_(default, param)
    
    return (param)
  }, names(expr[[2]]), expr[[2]], SIMPLIFY = FALSE)

  node$body = to_ast_(expr[[3]], node)
  return (node)
}


#' @export
to_ast_.name = function(expr, parent = NULL) {
  Symbol$new(parent, as.character(expr))
}


#' @export
`to_ast_.{` = function(expr, parent = NULL) {
  node = Bracket$new(parent)
  node$body = lapply(expr[-1], to_ast_, node)
  return (node)
}


#' @export
`to_ast_.(` = function(expr, parent) {
  node = Paren$new(parent)
  node$body = to_ast_(expr[[2]], node)
  return (node)
}


#' @export
to_ast_.NULL      = function(expr, parent = NULL) Null$new(parent)
#' @export
to_ast_.logical   = function(expr, parent = NULL) Logical$new(parent, expr)
#' @export
to_ast_.integer   = function(expr, parent = NULL) Integer$new(parent, expr)
#' @export
to_ast_.numeric   = function(expr, parent = NULL) Numeric$new(parent, expr)
#' @export
to_ast_.complex   = function(expr, parent = NULL) Complex$new(parent, expr)
#' @export
to_ast_.character = function(expr, parent = NULL) Character$new(parent, expr)


#' @export
to_ast_.default = function(expr, parent = NULL) {
  msg = sprintf("Cannot convert '%s' to an ASTNode.", class(expr)[1])
  stop(msg)
}
