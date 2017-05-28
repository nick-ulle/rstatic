context("ControlFlowGraph$copy")


test_that("copying copies fields with R6 objects", {
  x = ControlFlowGraph$new()
  x_priv = environment(x$initialize)$private

  y = x$copy()
  y_priv = environment(y$initialize)$private

  # -----
  expect_false(identical(x[[1]], y[[1]]))
})
