context("constant propagation")


test_that("constants are propagated in linear code", {
  cfg = toCFGq({
    x <- 1
    y <- x
  })

  const = propagateConstants(cfg)

  # -----
  expect_identical(const[["x_1"]], 1)
  expect_identical(const[["y_1"]], 1)
})


test_that("constants are propagated through phi functions", {
  cfg = toCFGq({
    if (TRUE) {
      x <- 1
    } else {
      x <- 2
    }
    y <- x
  })

  const = propagateConstants(cfg)

  # -----
  expect_identical(const[["x_1"]], 1)
  expect_identical(const[["y_1"]], 1)
})


test_that("unknowns are not propagated", {
  cfg = toCFGq({
    x = rnorm(1)
    if (x > 0) {
      x <- 1
    } else {
      x <- 2
    }
    y <- x
  })

  const = propagateConstants(cfg)

  # -----
  expect_identical(const[["x_1"]], NONCONST)
  expect_identical(const[["x_2"]], 1)
  expect_identical(const[["x_3"]], 2)
  expect_identical(const[["x_4"]], NONCONST)
  expect_identical(const[["y_1"]], NONCONST)
})


test_that("loop variables are nonconstant", {
  cfg = toCFGq({
    x = 42L
    for (i in 1:10) {
      x = i
    }
  })

  const = propagateConstants(cfg)

  # -----
  expect_identical(const[["._counter_i_2"]], NONCONST)

  expect_equal(const[["x_1"]], 42L)
  expect_identical(const[["x_2"]], NONCONST)
})


test_that("constants propagate through arithmetic", {
  cfg = toCFGq({
    x = 1
    y = x + 3
    z = (y * 7) / 4
  })

  const = propagateConstants(cfg)

  ## -----
  expect_equal(const[["x_1"]], 1L)
  expect_equal(const[["y_1"]], 4L)
  expect_equal(const[["z_1"]], 7L)
})
