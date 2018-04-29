context("compute_cfg")

test_that("if-statement graph has correct structure", {
  goal = igraph::make_empty_graph(n = 4)
  goal = goal + igraph::edges(c(1, 2, 1, 3, 2, 4, 3, 4))

  ast = If$new(
    Logical$new(TRUE),
    Brace$new(Assign$new(Symbol$new("x"), Integer$new(3L))),
    Brace$new(Assign$new(Symbol$new("x"), Integer$new(4L)))
  )

  result = to_blocks(ast, ssa = FALSE)
  g = compute_cfg(result)

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("pathological if-statement has correct structure", {
  goal = igraph::make_empty_graph(n = 4)
  goal = goal + igraph::edges(c(1, 2, 1, 3, 2, 4, 3, 4))

  ast = If$new(
    Symbol$new("x"),
    Brace$new()
  )

  result = to_blocks(ast, ssa = FALSE)
  g = compute_cfg(result)

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("if-statement with dual returns has correct structure", {
  goal = igraph::make_empty_graph(n = 4)
  goal = goal + igraph::edges(c(1, 2, 1, 3, 2, 4, 3, 4))

  ast = If$new(
    Call$new(">", list(Symbol$new("x"), Integer$new(0)) ),
    Brace$new(Return$new(Integer$new(3))),
    Brace$new(Return$new(Integer$new(-1)))
  )

  result = to_blocks(ast, ssa = FALSE)
  g = compute_cfg(result)

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("while-loop graph has correct structure", {
  goal = igraph::make_empty_graph(n = 4)
  goal = goal + igraph::edges(c(1,2, 2,1, 1,3, 3,4))

  ast = While$new(Logical$new(TRUE), Brace$new(Integer$new(42L)))

  result = to_blocks(ast, ssa = FALSE)
  g = compute_cfg(result)

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("for-loop graph has correct structure", {
  goal = igraph::make_empty_graph(n = 4)
  goal = goal + igraph::edges(c(1,2, 2,1, 1,3, 3,4))
  #goal = goal + igraph::edges(c(1,2, 2,3, 3,4, 4,5, 5,3, 3,6, 6,7))

  ast = For$new(Symbol$new("i"),
      Call$new(":", list(Integer$new(1L), Integer$new(3L))),
      Brace$new(Integer$new(42L))
  )

  result = to_blocks(ast, ssa = FALSE)
  g = compute_cfg(result)

  # -----
  expect_true(igraph::isomorphic(g, goal))
  # TODO: Check code generation in addition to graph.
})


