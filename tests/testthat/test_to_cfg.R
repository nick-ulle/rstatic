context("to_cfg")

test_that("if-statement graph has correct structure", {
  goal = igraph::make_empty_graph(n = 4)
  goal = goal + igraph::edges(c(1, 2, 1, 3, 2, 4, 3, 4))

  ast = If$new(
    Logical$new(TRUE),
    Assign$new(Symbol$new("x"), Integer$new(3L)),
    Assign$new(Symbol$new("x"), Integer$new(4L))
  )

  result = to_cfg(ast, ssa = FALSE)
  g = result$cfg$graph

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("if-statement with dual returns has correct structure", {
  goal = igraph::make_empty_graph(n = 4)
  goal = goal + igraph::edges(c(1, 2, 1, 3, 2, 4, 3, 4))

  ast = If$new(
    Call$new(">", list(Symbol$new("x"), Integer$new(0)) ),
    Return$new(Integer$new(3)),
    Return$new(Integer$new(-1))
  )

  result = to_cfg(ast, ssa = FALSE)
  g = result$cfg$graph

  # -----
  expect_true(igraph::isomorphic(g, goal))
})


test_that("while-loop graph has correct structure", {
  goal = igraph::make_empty_graph(n = 5)
  goal = goal + igraph::edges(c(1, 2, 2, 3, 2, 4, 3, 2, 4, 5))

  ast = While$new(Logical$new(TRUE), Integer$new(42L))

  result = to_cfg(ast, ssa = FALSE)
  g = result$cfg$graph

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

  result = to_cfg(ast, ssa = FALSE)
  g = result$cfg$graph

  # -----
  expect_true(igraph::isomorphic(g, goal))
  # TODO: Check code generation in addition to graph.
})


test_that("AST is copied when in_place = FALSE", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  node = to_cfg(ast, in_place = FALSE, ssa = FALSE)
  cfg = node$cfg

  # -----
  node = cfg[[1]]$body[[1]]
  expect_false(identical(ast, node))
  expect_false(identical(ast$read, node$read))
  expect_false(identical(ast$write, node$write))
})


test_that("AST is not copied when in_place = TRUE", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  node = to_cfg(ast, in_place = TRUE, ssa = FALSE)
  cfg = node$cfg

  # -----
  node = cfg[[1]]$body[[1]]
  expect_identical(ast, node)
  expect_identical(ast$read, node$read)
  expect_identical(ast$write, node$write)
})


test_that("nodes are reparented to containing BasicBlock", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  node = to_cfg(ast, ssa = FALSE)
  cfg = node$cfg

  # -----
  expect_identical(cfg[[1]], cfg[[1]]$body[[1]]$parent)
})


test_that("nested functions have CFG generated", {
  ast = quote_ast(
    function() {
      x = 1

      f = function(x) {
        x = 21
        x
      }

      f
    })

  result = to_cfg(ast)

  # -----
  expect_is(result$cfg[[1]]$body[[2]]$read$cfg, "ControlFlowGraph")
})
