context("to_cfg")

test_that("if-statement graph has correct structure", {
  goal = igraph::make_empty_graph(n = 5)
  goal = goal + igraph::edges(c(1, 2, 1, 3, 2, 4, 3, 4, 4, 5))

  ast = If$new(
    Logical$new(TRUE),
    Assign$new(Symbol$new("x"), Integer$new(3L)),
    Assign$new(Symbol$new("x"), Integer$new(4L))
  )

  result = to_cfg(ast, as_ssa = FALSE)
  g = result$graph

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("while-loop graph has correct structure", {
  goal = igraph::make_empty_graph(n = 5)
  goal = goal + igraph::edges(c(1, 2, 2, 3, 2, 4, 3, 2, 4, 5))

  ast = While$new(Logical$new(TRUE), Integer$new(42L))

  result = to_cfg(ast, as_ssa = FALSE)
  g = result$graph

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("for-loop graph has correct structure", {
  goal = igraph::make_empty_graph(n = 5)
  goal = goal + igraph::edges(c(1, 2, 2, 3, 2, 4, 3, 2, 4, 5))

  ast = For$new(Symbol$new("i"),
    Call$new(":", list(Integer$new(1L), Integer$new(3L))),
    Integer$new(42L)
  )

  result = to_cfg(ast, as_ssa = FALSE)
  g = result$graph

  # -----
  expect_true(igraph::isomorphic(g, goal))
  # TODO: Check code generation in addition to graph.
})


test_that("AST is copied when in_place = FALSE", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  cfg = to_cfg(ast, in_place = FALSE, as_ssa = FALSE)

  # -----
  node = cfg[[1]]$body[[1]]
  expect_false(identical(ast, node))
  expect_false(identical(ast$read, node$read))
  expect_false(identical(ast$write, node$write))
})


test_that("AST is not copied when in_place = TRUE", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  cfg = to_cfg(ast, in_place = TRUE, as_ssa = FALSE)

  # -----
  node = cfg[[1]]$body[[1]]
  expect_identical(ast, node)
  expect_identical(ast$read, node$read)
  expect_identical(ast$write, node$write)
})


test_that("nodes are reparented to containing BasicBlock", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  cfg = to_cfg(ast, as_ssa = FALSE)

  # -----
  expect_identical(cfg[[1]], cfg[[1]]$body[[1]]$parent)
})
