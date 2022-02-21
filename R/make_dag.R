#' Convert abstract syntax tree to directed acyclic graph
#'
#' @param x Abstract syntax tree from [infer_ast()]
#'
#' @examples
#' ast <- infer_ast("cos(2*x1 + x2)")
#' g <- make_dag(ast)
#' g
#'
#' if (interactive()) {
#'   plot(g)
#' }
#'
#' @importFrom igraph make_empty_graph vertex edge
#'
#' @export
make_dag <- function(x) {
  stopifnot(inherits(x, "taldi_AST"))

  res <- make_dag_worker(x)

  return(res)
}

make_dag_worker <- function(x,
                     graph = igraph::make_empty_graph(directed = TRUE),
                     ancestor_id = NULL,
                     env = new.env(parent = emptyenv())) {

  if (is.null(env[["id"]])) {
    env[["id"]] <- 2L

    ancestor_id <- 1L
    v <- igraph::vertex(1L)
    v$label <- "value"
    v$type <- "func"
    graph <- graph + v
  }

  id <- env[["id"]]
  env[["id"]] <- env[["id"]] + 1L

  v <- igraph::vertex(id)
  v$label <- x[[1L]]

  if (length(x) == 1L) {
    v$type <- attr(x, "type")
  } else {
    v$type <- attr(x[[1L]], "type")
  }


  graph <- graph + v

  if (is.null(ancestor_id)) {
    stop("Unexpected")
  }

  e <- igraph::edge(ancestor_id, id)
  graph <- graph + e

  if (length(x) >= 1L) {
    for (s in x[-1L]) {
      graph <- make_dag_worker(x = s,
                               graph = graph,
                               ancestor_id = id,
                               env = env)
    }
  }

  return(graph)
}
