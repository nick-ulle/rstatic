context("dominance_frontiers")

test_that("diamond graph", {
  g = igraph::make_empty_graph(n = 4) +
    igraph::edges(c(2,3, 2,4, 3,1, 4,1))

  result = dominance_frontiers(g)

  # -----
  expect_equal(vcount(result), vcount(g))
  expect_equal(ecount(result), 2)

  # Dominance should have
  # 3 --> 1, 4 --> 1
  expect_equal(result[3, 1], 1)
  expect_equal(result[4, 1], 1)
})
