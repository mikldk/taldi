stopifnot_ggraph_tidygraph <- function() {
  if (!require(ggraph, quietly = TRUE) || !require(tidygraph, quietly = TRUE)) {
    stop("Expected ggraph and tidygraph")
  }
}

# For internal use
.plot_dag <- function(g, add_node_text = TRUE) {
  stopifnot_ggraph_tidygraph()

  tg <- tidygraph::as_tbl_graph(g)
  l <- ggraph::create_layout(tg, layout = "sugiyama")

  p <- ggraph::ggraph(l) +
    ggraph::geom_edge_link(arrow = arrow(length = unit(0.5, 'cm')), end_cap = circle(0.5, 'cm')) +
    ggraph::geom_node_circle(aes(r = 0.1, fill = type)) +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme_bw() +
    ggraph::theme_graph()

  if (add_node_text) {
    p <- p + ggraph::geom_node_text(aes(label = label))
  }

  return(p)
}



#' Get symbol leaves
#'
#' @param dag in which to get symbol leaves
#'
#' @export
get_symbols <- function(dag) {
  is_symbol_leaf <- sapply(V(dag), function(x) {
    length(igraph::neighbors(dag, x, mode = "out")) == 0L &&
      igraph::get.vertex.attribute(dag, "type",  x) == "symbol"
  })

  return(V(dag)[is_symbol_leaf])
}
