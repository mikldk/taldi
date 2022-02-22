#' Perform reverse algorithmic differentiation
#'
#' @param dag in which to perform reverse algorithmic differentiation
#' @param values a named list of symbols and their values
#'
#' @examples
#' ast <- infer_ast("cos(2*x1 + x2) + x2^2")
#' dag <- make_dag(ast)
#' dag2 <- collect_leaves(dag)
#' dag2 <- reverse_ad(dag2, values = list(x1 = 1, x2 = 2))
#' dag2
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

  # includes init and binding
  dag2 <- forward_computation(dag, values)

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

  return(dag2)
}
