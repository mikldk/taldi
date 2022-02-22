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

# https://en.wikipedia.org/wiki/Differentiation_rules
get_deriv <- function(op, args, args_deriv) {
  stopifnot(length(args) == length(args_deriv))

  # Simple + - * /
  if (op == "+") {
    stopifnot(length(args) == 2L)
    return(c(args[1L] + args[2L],
             args_deriv[1L] + args_deriv[2L]))
  } else if (op == "-") {
    stopifnot(length(args) == 2L)
    return(c(args[1L] - args[2L],
             args_deriv[1L] - args_deriv[2L]))
  } else if (op == "*") {
    stopifnot(length(args) == 2L)
    return(c(args[1L] * args[2L],
             args_deriv[1L]*args[2L] + args[1L]*args_deriv[2L]))
  } else if (op == "/") {
    stopifnot(length(args) == 2L)
    return(c(args[1L] * args[2L],
             (args_deriv[1L]*args[2L] - args[1L]*args_deriv[2L])/(args[2L]^2)))
  }

  # Unary trigonometry functions
  else if (op == "cos") {
    stopifnot(length(args) == 1L)
    return(c(cos(args[1L]),
             -sin(args[1L])*args_deriv[1L]))
  } else if (op == "sin") {
    stopifnot(length(args) == 1L)
    return(c(sin(args[1L]),
             cos(args[1L])*args_deriv[1L]))
  }
}

#' Initiate graph for computations
#'
#' @param dag to initiate
#'
#' @export
init_graph <- function(dag) {
  #dag2 <- igraph::set_vertex_attr(dag, "_ad_value", value = NA)

  dag2 <- dag

  for (v in V(dag2)) {
    dag2 <- igraph::set_vertex_attr(dag2, "_ad_value", index = v, value = NA)
  }

  return(dag2)
}

require_graph_initiated <- function(dag) {
  vls <- igraph::vertex_attr(dag, "_ad_value")

  if (is.null(vls)) {
    stop("DAG not yet initiated, use init_graph()")
  }

  return(TRUE)
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
    dag2 <- igraph::set_vertex_attr(dag2, "_ad_value", index = v, value = as.numeric(lbl))
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

    dag2 <- igraph::set_vertex_attr(dag2, "_ad_value", index = v, value = values[idx])
  }

  return(dag2)
}

forward_computation <- function(dag) {

}

#' Perform reverse algorithmic differentiation
#'
#' @param dag in which to perform reverse algorithmic differentiation
#' @param values a named list of symbols and their values
#'
#' @examples
#' ast <- infer_ast("cos(2*x1 + x2)")
#' dag <- make_dag(ast)
#' #ad <- reverse_ad(dag)
#' #ad
#'
#' @importFrom igraph bfs set_vertex_attr neighbors V E
#'
#' @export
reverse_ad <- function(dag, values) {

  # 1) Specify all variables
  # 2) Compute a forward-pass with inputs values that flows through the computations to get the output. Store each node's value within the node.
  # 3) Compute a backward-pass to compute gradients, beginning from a gradient of 1 at the final node (the output).

  require_unique_symbol_leaves(dag)

  dag2 <- dag
  dag2 <- init_graph(dag2)
  dag2 <- bind_literals(dag2)
  dag2 <- bind_symbols(dag2, values)

  # if (is.null(symbols)) {
  #   symbols <- get_symbols(dag)
  # }
  #
  # # igraph::vertex_attr(dag, "label",  symbols)
  # # symbols$name
  # dag_ad <- dag
  # dag_ad <- igraph::set_vertex_attr(dag_ad, "deriv", value = 1)
  #
  # # ggdag(dag_ad)
  # # ggdag(dag_ad, add_node_text = FALSE) + ggraph::geom_node_text(aes(label = paste0(label, ": ", deriv)))
  #
  # search_res <- igraph::bfs(dag_ad, root = 1L, neimode = "out")
  # #search_res
  #
  # vids <- search_res$order # Exclude root ("value")
  # v <- igraph::V(dag_ad)[ vids[1L] ]
  #
  # for (i in 1L+seq_len(length(vids) - 1L)) {
  #   u <- igraph::V(dag_ad)[ vids[i] ]
  #   e_id <- get.edge.ids(dag_ad, c(v, u))
  #   e <- igraph::E(dag_ad)[e_id]
  #
  #   #cat(u$name, ": ", u$label, "\n")
  #   cat(v$label, " (", v$name, ") -> ", u$label, " (", u$name, ")\n", sep = "")
  #
  #   v_pa <- igraph::neighbors(dag, v, mode = "in")
  #
  #   v <- u
  # }
  #
  #
  # cbf <- function(graph, data, extra) {
  #   #print(data)
  #   #print(data["vid"])
  #   #print(str(data))
  #
  #   v <- igraph::V(graph)[igraph::V(graph)$id == data["vid"]]
  #   print(v)
  #   #cat("Visiting ", v$name, " (", v$label, ")\n")
  #
  #   FALSE
  # }
  #
  # search_res <- igraph::bfs(dag_ad, root = 1L, neimode = "out", callback = cbf)
  # search_res

}
