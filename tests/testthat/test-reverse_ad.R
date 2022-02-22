test_that("require_unique_symbol_leaves", {
  ast <- infer_ast("cos(2*x1 + x2) + x2^2")
  dag <- make_dag(ast)
  expect_error(require_unique_symbol_leaves(dag))

  dag2 <- collect_leaves(dag)
  expect_true(require_unique_symbol_leaves(dag2))
})

test_that("get_deriv", {

})

test_that("init_graph", {
  ast <- infer_ast("cos(2*x1 + x2) + x2^2")
  dag <- make_dag(ast)
  dag <- collect_leaves(dag)
  dag <- init_graph(dag)

  vls <- igraph::vertex_attr(dag, "_ad_value")
  expect_true(isTRUE(all(is.na(vls))))
})

test_that("bind_literals", {
  ast <- infer_ast("cos(2*x1 + x2) + x2^2")
  dag <- make_dag(ast)
  dag <- collect_leaves(dag)
  #ggdag(dag)

  expect_error(require_graph_initiated(dag))

  dag <- init_graph(dag)
  expect_true(require_graph_initiated(dag))

  dag2 <- bind_literals(dag)
  #ggdag(dag2)

  expect_true(!isTRUE(all(is.na(unlist(igraph::vertex_attr(dag2, "_ad_value"))))))
})

test_that("bind_symbols", {
  ast <- infer_ast("cos(2*x1 + x2) + x2^2")
  dag <- make_dag(ast)
  dag <- collect_leaves(dag)
  #ggdag(dag)

  expect_error(require_graph_initiated(dag))

  dag <- init_graph(dag)
  expect_true(require_graph_initiated(dag))

  #ggdag(dag)
  expect_error(bind_symbols(dag, values = list(x1 = 1))) # x2 missing

  dag2 <- bind_symbols(dag, values = list(x1 = 1, x2 = 2))
  #ggdag(dag2)

  expect_true(!isTRUE(all(is.na(unlist(igraph::vertex_attr(dag2, "_ad_value"))))))
})

test_that("require_leaves_bound", {
  ast <- infer_ast("cos(2*x1 + x2) + x2^2")
  dag <- make_dag(ast)
  dag2 <- dag

  expect_error(require_leaves_bound(dag2))

  dag2 <- collect_leaves(dag2)

  expect_error(require_leaves_bound(dag2))

  dag2 <- init_graph(dag2)
  dag2 <- bind_symbols(dag2, values = list(x1 = 1, x2 = 2))
  dag2 <- bind_literals(dag2)

  expect_true(require_leaves_bound(dag2))
})
