context("SSA graph")

test_that("nodes are added for defs", {
  exp = quote(
    x <- 1
  )

  cfg = to_cfg(exp)
  ssa = cfg$ssa

  # -----
  expect_is(ssa[["x_1"]], "Assign")
})


test_that("nodes are added for uses", {
  exp = quote(
    foo(x)
  )

  cfg = to_cfg(exp)
  ssa = cfg$ssa

  # -----
  expect_is(ssa[["%1"]], "Call")
})


test_that("edges are added for defs that are also uses", {
  # There should be a link in the SSA graph from x_1 to y_1
  exp = quote({
    x = 1
    y = x
    # FIXME:
    # return (x)
  })

  cfg = to_cfg(exp)
  ssa = cfg$ssa

  # -----
  expect_is(ssa[["x_1"]], "Assign")
  expect_is(ssa[["y_1"]], "Assign")

  expect_true(ssa$graph[from = "x_1", to = "y_1"] == 1)
})


test_that("nodes are added for uses that are not defs", {
  exp = quote({
    x <- 1
    foo(x)
  })

  cfg = to_cfg(exp)
  ssa = cfg$ssa

  # -----
  expect_is(ssa[["x_1"]], "Assign")
  expect_is(ssa[["%1"]], "Call")

  expect_true(ssa$graph[from = "x_1", to = "%1"] == 1)
})


test_that("nodes and edges are added for phi-functions", {
  exp = quote({
    if (TRUE)
      x = 1
    else
      x = 2
    y = x
  })

  cfg = to_cfg(exp)
  ssa = cfg$ssa

  # -----
  expect_is(ssa[["x_1"]], "Assign")
  expect_is(ssa[["x_2"]], "Assign")
  expect_is(ssa[["x_3"]], "Phi")
  expect_is(ssa[["y_1"]], "Assign")

  expect_true(ssa$graph[from = "x_1", to = "x_3"] == 1)
  expect_true(ssa$graph[from = "x_2", to = "x_3"] == 1)
  expect_true(ssa$graph[from = "x_3", to = "y_1"] == 1)
})


test_that("nodes and edges are added for parameters", {
  fn = function(x) {
    y = x
  }

  cfg = to_cfg(fn)
  ssa = cfg$ssa

  # -----
  expect_is(ssa[["x_1"]], "Parameter")
  expect_is(ssa[["y_1"]], "Assign")

  expect_true(ssa$graph[from = "x_1", to = "y_1"] == 1)
})


test_that("globals", {
  exp = quote(y <- x)

  # FIXME:
  #cfg = to_cfg(fn)
  #ssa = cfg$ssa

  # -----
  #browser()
})
