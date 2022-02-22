#' Plot DAG
#'
#' @param dag to plot
#' @param add_node_text add text to nodes
#' @param r radius of nodes
#' @param arrow_size size of arrows
#' @param end_cap cap end to not hide arrow
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
#' dag <- forward_computation(dag, values = list(x1 = 1, x2 = 2))
#'
#' if (interactive()) {
#'   ggdag(dag)
#' }
#'
#' @importFrom tidygraph as_tbl_graph as_tibble
#' @importFrom ggraph ggraph create_layout geom_edge_link geom_node_circle theme_graph circle geom_node_text
#' @importFrom ggplot2 labs theme_bw arrow unit aes
#' @importFrom rlang .data
#'
#' @export
ggdag <- function(dag, add_node_text = TRUE,
                  r = 0.3,
                  arrow_size = ggplot2::unit(0.25, 'cm'),
                  end_cap = ggraph::circle(0.85, 'cm')) {

  tg <- tidygraph::as_tbl_graph(dag)

  has_ad_value <- FALSE

  if (add_node_text) {
    attr(tg, "active") <- "nodes"
    cnms <- colnames(tidygraph::as_tibble(tg))
    has_ad_value <- "_ad_value" %in% cnms

    if (has_ad_value) {
      tg <- tidygraph::mutate(tg, `_ad_value` = as.numeric(unlist(.data$`_ad_value`)))

      #tg <- tidygraph::mutate(tg, `_ad_value_pretty` = round(`_ad_value`, 2))
      #tg <- tidygraph::mutate(tg, `_ad_value_pretty` = lapply(`_ad_value`, formatC))
    }
  }

  l <- ggraph::create_layout(tg, layout = "sugiyama")

  p <- ggraph::ggraph(l) +
    ggraph::geom_edge_link(arrow = ggplot2::arrow(length = arrow_size), end_cap = end_cap) +
    ggraph::geom_node_circle(ggplot2::aes(r = r, fill = .data$type)) +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme_bw() +
    ggraph::theme_graph()

  if (add_node_text) {
    if (has_ad_value) {
      #p <- p + ggraph::geom_node_text(ggplot2::aes(label = paste0(.data$label, "\n", .data$`_ad_value`)))
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = paste0(.data$label, "\n", lapply(.data$`_ad_value`, formatC))))
    } else {
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = .data$label))
    }
  }

  return(p)
}



#' Get symbol leaves
#'
#' @param dag in which to get symbol leaves
#'
#' @importFrom igraph V neighbors vertex_attr
#'
#' @export
get_symbols <- function(dag) {
  is_symbol_leaf <- sapply(igraph::V(dag), function(x) {
    length(igraph::neighbors(dag, x, mode = "out")) == 0L &&
      igraph::vertex_attr(dag, "type",  x) == "symbol"
  })

  return(igraph::V(dag)[is_symbol_leaf])
}



#' Get leaves
#'
#' @param dag to get leaves from
#'
#' @importFrom igraph V neighbors
#'
#' @export
get_leaves <- function(dag) {
  is_leaf <- sapply(igraph::V(dag), function(x) {
    length(igraph::neighbors(dag, x, mode = "out")) == 0L
  })

  return(is_leaf)
}

#' Get root (value) node
#'
#' @param dag to get root of
#'
#' @importFrom igraph V
#'
#' @export
get_root <- function(dag) {
  return(igraph::V(dag)[1L]) # By contract made in make_dag()/make_dag_worker()
}


