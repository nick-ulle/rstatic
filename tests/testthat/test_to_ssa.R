context("to_ssa")


test_that("CFG is copied when in_place = FALSE", {
  cfg = CFGraph$new()

  result = to_ssa(cfg, in_place = FALSE)

  # -----
  expect_false(identical(cfg, result))
})


test_that("CFG is not copied when in_place = FALSE", {
  cfg = CFGraph$new()

  result = to_ssa(cfg, in_place = TRUE)

  # -----
  expect_identical(cfg, result)
})
