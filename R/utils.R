#' Plot DAG
#'
#' @param dag to plot
#' @param add_node_text add text to nodes
#' @param add_value add the value to node text
#' @param add_deriv add the derivative to node text
#' @param r radius of nodes
#' @param arrow_size size of arrows
#' @param end_cap cap end to not hide arrow
#' @param lineheight of node text
#' @param value_prefix Prefix when printing AD value
#' @param deriv_prefix Prefix when printing AD derivate
#' @param format_args Passed on to `formatC` as dots
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
#'   ggdag(dag, add_value = TRUE)
#'   ggdag(dag, add_value = TRUE, add_deriv = TRUE)
#'   ggdag(dag, add_value = TRUE, add_deriv = TRUE, format_args = list(digits = 2))
#' }
#'
#' @importFrom tidygraph as_tbl_graph as_tibble
#' @importFrom ggraph ggraph create_layout geom_edge_link geom_node_circle theme_graph circle geom_node_text
#' @importFrom ggplot2 labs theme_bw arrow unit aes
#' @importFrom rlang .data
#'
#' @export
ggdag <- function(dag,
                  add_node_text = TRUE,
                  add_value = FALSE,
                  add_deriv = FALSE,
                  r = 0.3,
                  arrow_size = ggplot2::unit(0.25, 'cm'),
                  end_cap = ggraph::circle(0.85, 'cm'),
                  lineheight = 0.85,
                  value_prefix = "v = ",
                  deriv_prefix = "d = ",
                  format_args = list()) {

  formatCargs <- function(x) {
    args <- format_args
    args$x <- x
    do.call(formatC, args)
  }

  tg <- tidygraph::as_tbl_graph(dag)

  has_value <- FALSE
  has_deriv <- FALSE

  if (add_value || add_deriv) {
    add_node_text <- TRUE
  }

  if (add_node_text) {
    attr(tg, "active") <- "nodes"
    cnms <- colnames(tidygraph::as_tibble(tg))

    has_value <- "_value" %in% cnms
    if (has_value) {
      tg <- tidygraph::mutate(tg, `_value` = as.numeric(unlist(.data$`_value`)))
    }

    has_deriv <- "_ad_deriv" %in% cnms
    if (has_deriv) {
      tg <- tidygraph::mutate(tg, `_ad_deriv` = as.numeric(unlist(.data$`_ad_deriv`)))
    }

    # Preparing label,
    # start with only label e.g. add_value == FALSE & add_deriv == FALSE

    tg <- tidygraph::mutate(tg, `node_label` = .data$label)

    if (has_value & add_value) {
      tg <- tidygraph::mutate(tg, `node_label` = paste0(.data$node_label, "\n", value_prefix, lapply(.data$`_value`, formatCargs)))
    }

    if (has_deriv & add_deriv) {
      tg <- tidygraph::mutate(tg, `node_label` = paste0(.data$node_label, "\n", deriv_prefix, lapply(.data$`_ad_deriv`, formatCargs)))
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
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = .data$node_label), lineheight = lineheight)
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


