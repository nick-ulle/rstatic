context("usedef")


test_that("usedef detected for simple assignment", {
  cfg = to_cfg(to_astq(
      x <- y
  ))

  result = cfg$usedef

  # -----
  expect_named(result, c("y", "x_1"), ignore.order = TRUE)

  x_1 = result$x_1
  expect_named(x_1, c("def", "use"))
  expect_is(x_1$def, "Assign")
  expect_is(x_1$use, "list")
  expect_equal(length(x_1$use), 0L)

  y = result$y
  expect_named(y, c("def", "use"))
  expect_null(y$def)
  expect_is(y$use, "list")
  expect_equal(length(y$use), 1)
  expect_is(y$use[[1]], "Symbol")
})

