#' Infer abstract syntax tree
#'
#' @param x Expression to parse (`language` or `chr`)
#'
#' @examples
#' e <- quote(cos(2*x1 + x2))
#' t1 <- infer_ast(e)
#' t1
#'
#' t2 <- infer_ast("cos(2*x1 + x2)")
#' t2
#'
#' all.equal(t1, t2)
#'
#' @importFrom rlang quo_squash is_syntactic_literal is_symbol
#'
#' @export
infer_ast <- function(x) {
  res <- infer_ast_worker(x)
  class(res) <- "taldi_AST"
  return(res)
}

infer_ast_worker <- function(x) {
  if (is.character(x)) {
    x <- parse(text = x)[[1L]]
  }

  if (rlang::is_quosure(x)) {
    x <- rlang::quo_squash(x)
  }

  if (rlang::is_syntactic_literal(x)) {
    if (is.character(x)) {
      z <- x
      attr(z, "type") <- "char"
      return(z)
    } else {
      z <- x
      attr(z, "type") <- "literal"
      return(z)
    }
  } else if (rlang::is_symbol(x)) {
    x <- as.character(x)
    if (!(make.names(x) == x)) { # /, *, ...
      if (x == "(") {
        return(NULL) # Remove
      }
      z <- x
      attr(z, "type") <- "op"
      return(z)
    } else if (exists(x) && is.function(get(x))) { # exp, sin, ...
      z <- x
      attr(z, "type") <- "func"
      return(z)
    } else {
      z <- x
      attr(z, "type") <- "symbol"
      return(z)
    }
  }

  return(lapply(x, infer_ast_worker))
}
