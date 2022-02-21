#' Collect symbol leaves
#'
#' @param dag in which same symbol leaves should be collected
#'
#' @examples
#' symlist <- infer_ast("cos(2*x1 + x2) + x2^2")
#' dag <- make_dag(symlist)
#' if (interactive()) {
#'   plot(dag)
#' }
#'
#' dag2 <- collect_leaves(dag)
#' if (interactive()) {
#'   plot(dag2)
#' }
#'
#' @importFrom igraph vertex edge get.vertex.attribute neighbors
#' @export
collect_leaves <- function(dag) {

  is_symbol_leaf <- sapply(V(dag), function(x) {
    length(igraph::neighbors(dag, x, mode = "out")) == 0L &&
      igraph::get.vertex.attribute(dag, "type",  x) == "symbol"
  })

  idx <- which(is_symbol_leaf)
  lbls <- igraph::get.vertex.attribute(dag, "label",  V(dag)[is_symbol_leaf])

  smbl_lst <- split(idx, lbls)

  dag2 <- dag

  for (i in seq_along(smbl_lst)) {
    smb_i <- smbl_lst[[i]]

    if (length(smb_i) <= 1L) {
      next
    }

    #Take #1 as new node, let the rest merge to that
    v <- V(dag)[ smb_i[1L] ]
    v_pa <- igraph::neighbors(dag, v, mode = "in")

    # k in 2, 3, ...n
    for (k in (1L + seq_len(length(smb_i) - 1L))) {
      u <- V(dag)[ smb_i[k] ]

      u_pa <- igraph::neighbors(dag, u, mode = "in")

      for (j in seq_along(u_pa)) {
        dag2 <- dag2 + igraph::edge(u_pa[[j]], v)
      }

      dag2 <- dag2 - igraph::vertex(u)
    }
  }

  return(dag2)
}
