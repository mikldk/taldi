require_unique_symbol_leaves <- function(dag) {
  is_symbol_leaf <- sapply(V(dag), function(x) {
    length(igraph::neighbors(dag, x, mode = "out")) == 0L &&
      igraph::get.vertex.attribute(dag, "type",  x) == "symbol"
  })

  lbls <- igraph::get.vertex.attribute(dag, "label",  V(dag)[is_symbol_leaf])

  if (length(lbls) != length(unique(lbls))) {
    stop("Symbol leaves must be unique; please use collect_leaves() to obtain this.")
  }
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


#' Perform reverse algorithmic differentiation
#'
#' @param dag in which to perform reverse algorithmic differentiation
#' @param symbols which symbols to differentiate with respect to (NULL means all)
#'
#' @examples
#' ast <- infer_ast("cos(2*x1 + x2)")
#' dag <- make_dag(ast)
#' ad <- reverse_ad(dag)
#' ad
#'
#' @importFrom igraph bfs set_vertex_attr neighbors
#'
#' @export
reverse_ad <- function(dag, symbols = NULL) {
  require_unique_symbol_leaves(dag)

  if (is.null(symbols)) {
    symbols <- get_symbols(dag)
  }

  # igraph::get.vertex.attribute(dag, "label",  symbols)
  # symbols$name
  dag_ad <- dag
  dag_ad <- igraph::set_vertex_attr(dag_ad, "deriv", value = 1)

  # .plot_dag(dag_ad)
  # .plot_dag(dag_ad, add_node_text = FALSE) + ggraph::geom_node_text(aes(label = paste0(label, ": ", deriv)))

  search_res <- igraph::bfs(dag_ad, root = 1L, neimode = "out")
  #search_res

  vids <- search_res$order # Exclude root ("value")
  v <- V(dag_ad)[ vids[1L] ]

  for (i in 1L+seq_len(length(vids) - 1L)) {
    u <- V(dag_ad)[ vids[i] ]
    e_id <- get.edge.ids(dag_ad, c(v, u))
    e <- E(dag_ad)[e_id]

    #cat(u$name, ": ", u$label, "\n")
    cat(v$label, " (", v$name, ") -> ", u$label, " (", u$name, ")\n", sep = "")

    v_pa <- igraph::neighbors(dag, v, mode = "in")

    v <- u
  }`


  cbf <- function(graph, data, extra) {
    #print(data)
    #print(data["vid"])
    #print(str(data))

    v <- V(graph)[V(graph)$id == data["vid"]]
    print(v)
    #cat("Visiting ", v$name, " (", v$label, ")\n")

    FALSE
  }

  search_res <- bfs(dag_ad, root = 1L, neimode = "out", callback = cbf)
  search_res

}
