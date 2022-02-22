#' Get AD value of node
#'
#' @param dag to look `nodes` up in
#' @param nodes to get AD value from
#'
#' @importFrom igraph vertex_attr
#'
#' @export
get_ad_values <- function(dag, nodes) {
  unlist(igraph::vertex_attr(dag, "_ad_value", index = nodes))
}

do_calculation <- function(op, args) {
  # Simple + - * /
  if (op == "+") {
    stopifnot(length(args) == 2L)
    return(args[1L] + args[2L])

  } else if (op == "-") {
    stopifnot(length(args) == 2L)
    return(args[1L] - args[2L])

  } else if (op == "*") {
    stopifnot(length(args) == 2L)
    return(args[1L] * args[2L])

  } else if (op == "/") {
    stopifnot(length(args) == 2L)
    return(args[1L] / args[2L])

  } else if (op == "^") {
    stopifnot(length(args) == 2L)
    return(args[1L]^args[2L])
  }

  # Unary trigonometry functions
  else if (op == "cos") {
    stopifnot(length(args) == 1L)
    return(cos(args[1L]))

  } else if (op == "sin") {
    stopifnot(length(args) == 1L)
    return(sin(args[1L]))

  } else if (op == "exp") {
    stopifnot(length(args) == 1L)
    return(exp(args[1L]))
  }

  return(NA)
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

#' Performs the function calculation forwards in graph
#'
#' @param dag to perform calculations in
#' @param values a named list of symbols and their values
#'
#' @examples
#' ast <- infer_ast("cos(2*x1 + x2) + x2^2")
#' dag <- make_dag(ast)
#' dag <- collect_leaves(dag)
#' dag <- forward_computation(dag, values = list(x1 = 1, x2 = 2))
#' dag
#'
#' eval(expression(cos(2*x1 + x2) + x2^2), list(x1 = 1, x2 = 2))
#'
#' @importFrom igraph bfs set_vertex_attr neighbors V E
#'
#' @export
forward_computation <- function(dag, values) {
  require_unique_symbol_leaves(dag)

  dag2 <- dag
  dag2 <- init_graph(dag2)
  dag2 <- bind_literals(dag2)
  dag2 <- bind_symbols(dag2, values)

  require_leaves_bound(dag2)

  is_leaf <- get_leaves(dag2)
  leaves <- igraph::V(dag2)[is_leaf]

  # ggdag(dag2)
  dag3 <- forward_computation_worker(dag2, leaves)
  # ggdag(dag3)
  dag3 <- fill_value_root(dag3)
  # ggdag(dag3)

  return(dag3)
}

fill_value_root <- function(dag) {
  root <- igraph::V(dag)[1L]
  child <- igraph::neighbors(dag, root, mode = "out")
  stopifnot(length(child) == 1L)

  child_val <- igraph::vertex_attr(dag, "_ad_value", index = child)[[1L]]

  dag <- igraph::set_vertex_attr(dag, "_ad_value", index = root, value = child_val)

  return(dag)
}

forward_computation_worker <- function(dag, nodes) {
  if (is.null(nodes)) {
    return(dag)
  }

  for (v in nodes) {
    v_pa <- igraph::neighbors(dag, v, mode = "in")

    for (u in v_pa) {
      dag <- compute_node(dag, u)
    }

    dag <- forward_computation_worker(dag, v_pa)
  }

  return(dag)
}

compute_node <- function(dag, node) {
  stopifnot(length(node) == 1L)

  # Node already computed
  if (!is.na(igraph::vertex_attr(dag, "_ad_value", index = node)[[1L]])) {
    return(dag)
  }

  u_children <- igraph::neighbors(dag, node, mode = "out")
  args <- get_ad_values(dag, u_children)

  op <- igraph::vertex_attr(dag, "label", node)

  val <- do_calculation(op, args)
  dag <- igraph::set_vertex_attr(dag, "_ad_value", index = node, value = val)

  return(dag)
}


