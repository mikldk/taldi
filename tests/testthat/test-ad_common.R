test_that("get_values", {
  ast <- infer_ast("cos(2*x1 + x2) + x2^2")
  dag <- make_dag(ast)
  dag <- collect_leaves(dag)
  dag <- init_graph(dag)
  dag <- bind_literals(dag)
  dag <- bind_symbols(dag, values = list(x1 = 1, x2 = 2.2))

  v <- igraph::V(dag)[igraph::vertex_attr(dag, "label") == "x2"]

  expect_equal(2.2, get_values(dag, v))
})

test_that("do_calculation", {
  expect_equal(do_calculation("+", c(1, 3)), 1+3)
  expect_equal(do_calculation("-", c(1, 3)), 1-3)
  expect_equal(do_calculation("*", c(1.4, 4)), 1.4*4)
  expect_equal(do_calculation("/", c(2, 3)), 2/3)
  expect_equal(do_calculation("^", c(2, 2.2)), 2^2.2)


  expect_equal(do_calculation("cos", pi), cos(pi))
  expect_equal(do_calculation("sin", pi), sin(pi))
  expect_equal(do_calculation("exp", 2), exp(2))
})

#test_that("get_deriv", {
#
#})



test_that("forward_computation", {
  e <- quote(cos(2*x1 + x2) + x2^2)
  vals <- list(x1 = 1.1, x2 = 2.2)

  ast <- infer_ast(e)
  dag <- make_dag(ast)
  dag <- collect_leaves(dag)
  dag <- forward_computation(dag, values = vals)

  v1 <- eval(e, vals)

  root <- get_root(dag)
  v2 <- get_values(dag, root)

  expect_equal(v1, v2)
})

