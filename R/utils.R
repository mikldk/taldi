#' Plot DAG
#'
#' @param dag to plot
#' @param add_node_text add text to nodes
#'
#' @importFrom tidygraph as_tbl_graph as_tibble
#' @importFrom ggraph ggraph create_layout geom_edge_link geom_node_circle theme_graph circle geom_node_text
#' @importFrom ggplot2 labs theme_bw arrow unit aes
#' @importFrom rlang .data
#'
#' @export
ggdag <- function(dag, add_node_text = TRUE) {
  tg <- tidygraph::as_tbl_graph(dag)

  has_ad_value <- FALSE

  if (add_node_text) {
    attr(tg, "active") <- "nodes"
    cnms <- colnames(tidygraph::as_tibble(tg))
    has_ad_value <- "_ad_value" %in% cnms

    if (has_ad_value) {
      tg <- tidygraph::mutate(tg, `_ad_value` = unlist(`_ad_value`))

      #tg <- tidygraph::mutate(tg, `_ad_value_pretty` = round(`_ad_value`, 2))
      #tg <- tidygraph::mutate(tg, `_ad_value_pretty` = lapply(`_ad_value`, formatC))
    }
  }

  l <- ggraph::create_layout(tg, layout = "sugiyama")

  p <- ggraph::ggraph(l) +
    ggraph::geom_edge_link(arrow = ggplot2::arrow(length = ggplot2::unit(0.5, 'cm')), end_cap = ggraph::circle(0.5, 'cm')) +
    ggraph::geom_node_circle(ggplot2::aes(r = 0.1, fill = .data$type)) +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme_bw() +
    ggraph::theme_graph()

  if (add_node_text) {
    if (has_ad_value) {
      #p <- p + ggraph::geom_node_text(ggplot2::aes(label = paste0(.data$label, "=", .data$`_ad_value`)))
      p <- p + ggraph::geom_node_text(ggplot2::aes(label = paste0(.data$label, "=", lapply(.data$`_ad_value`, formatC))))
      #p <- p + ggraph::geom_node_text(ggplot2::aes(label = paste0(.data$label, "=", lapply(.data$`_ad_value_pretty`, formatC))))
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




get_leaves <- function(dag) {
  is_leaf <- sapply(igraph::V(dag), function(x) {
    length(igraph::neighbors(dag, x, mode = "out")) == 0L
  })

  return(is_leaf)
}
