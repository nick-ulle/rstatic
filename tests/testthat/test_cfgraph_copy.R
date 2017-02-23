context("CFGraph$copy")


test_that("copying copies fields with R6 objects", {
  x = CFGraph$new()
  x_priv = environment(x$initialize)$private

  y = x$copy()
  y_priv = environment(y$initialize)$private

  # -----
  expect_false(identical(x$blocks[[1]], y$blocks[[1]]))
  expect_false(identical(x_priv$loop_stack, y_priv$loop_stack))
})
