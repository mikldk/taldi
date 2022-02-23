require_unique_symbol_leaves <- function(dag) {
  is_symbol_leaf <- sapply(V(dag), function(x) {
    length(igraph::neighbors(dag, x, mode = "out")) == 0L &&
      igraph::vertex_attr(dag, "type",  x) == "symbol"
  })

  lbls <- igraph::vertex_attr(dag, "label",  V(dag)[is_symbol_leaf])

  if (length(lbls) != length(unique(lbls))) {
    stop("Symbol leaves must be unique; please use collect_leaves() to obtain this.")
  }

  return(TRUE)
}

# By contract only checks _value (and not also _ad_deriv)
require_graph_initiated <- function(dag) {
  vls <- igraph::vertex_attr(dag, "_value")

  if (is.null(vls)) {
    stop("DAG not yet initiated, use init_graph()")
  }

  return(TRUE)
}

# By contract only checks _value (and not also _ad_deriv)
require_leaves_bound <- function(dag) {
  require_graph_initiated(dag)

  is_leaf <- get_leaves(dag)

  vls <- igraph::vertex_attr(dag, "_value")
  vls_leaves <- unlist(vls[is_leaf])

  if (any(is.na(vls_leaves))) {
    stop("Some leaves were NA.")
  }

  if (!isTRUE(all(is.numeric(vls_leaves)))) {
    stop("Not all leaves were numeric.")
  }

  if (any(is.infinite(vls_leaves))) {
    stop("Some leaves were not finite.")
  }

  return(TRUE)
}


#' Initiate graph for computations
#'
#' @param dag to initiate
#'
#' @export
init_graph <- function(dag) {
  dag2 <- dag

  for (v in V(dag2)) {
    dag2 <- igraph::set_vertex_attr(dag2, "_value", index = v, value = NA)
    dag2 <- igraph::set_vertex_attr(dag2, "_ad_deriv", index = v, value = NA)
  }

  return(dag2)
}

#' Bind literals
#'
#' @param dag to bind literals in
#'
#' @examples
#' ast <- infer_ast("cos(2*x1 + x2) + x2^2")
#' dag <- make_dag(ast)
#' dag <- collect_leaves(dag)
#'
#' if (interactive()) {
#'   ggdag(dag)
#' }
#'
#' dag2 <- init_graph(dag)
#'
#' if (interactive()) {
#'   ggdag(dag2)
#' }
#'
#' dag2 <- bind_literals(dag2)
#'
#' if (interactive()) {
#'   ggdag(dag2)
#' }
#'
#' @importFrom igraph vertex_attr vertex_attr V
#'
#' @export
bind_literals <- function(dag) {
  require_graph_initiated(dag)

  dag2 <- dag

  for (v in igraph::V(dag2)) {
    if (igraph::vertex_attr(dag2, "type", index = v) != "literal") {
      next
    }

    lbl <- igraph::vertex_attr(dag2, "label", index = v)
    dag2 <- igraph::set_vertex_attr(dag2, "_value", index = v, value = as.numeric(lbl))
  }

  return(dag2)
}

#' Bind symbols
#'
#' @param dag to bind symbols in
#' @param values to bind for symbols, named list
#'
#' @examples
#' ast <- infer_ast("cos(2*x1 + x2) + x2^2")
#' dag <- make_dag(ast)
#' dag <- collect_leaves(dag)
#'
#' if (interactive()) {
#'   ggdag(dag)
#' }
#'
#' dag2 <- init_graph(dag)
#'
#' if (interactive()) {
#'   ggdag(dag2)
#' }
#'
#' dag2 <- bind_symbols(dag2, values = list(x1 = 1, x2 = 2))
#'
#' if (interactive()) {
#'   ggdag(dag2)
#' }
#'
#' @importFrom igraph vertex_attr vertex_attr V
#'
#' @export
bind_symbols <- function(dag, values) {
  require_graph_initiated(dag)

  vnms <- names(values)

  if (length(vnms) != length(unique(vnms))) {
    stop("Some symbols had multiple values")
  }

  dag2 <- dag

  # Symbols
  for (v in igraph::V(dag2)) {
    if (igraph::vertex_attr(dag2, "type", index = v) != "symbol") {
      next
    }

    lbl <- igraph::vertex_attr(dag2, "label", index = v)
    idx <- which(names(values) == lbl)

    if (length(idx) == 0L) {
      stop("The symbol '", lbl, "' was not provided a value")
    } else if (length(idx) > 1L) {
      stop("The symbol '", lbl, "' was not expected to have multiple values")
    }

    dag2 <- igraph::set_vertex_attr(dag2, "_value", index = v, value = values[idx])
  }

  return(dag2)
}

