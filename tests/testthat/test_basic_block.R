context("BasicBlock")


test_that("$new() sets parents on body nodes", {
  x = BasicBlock$new(list(
    Assign$new(Symbol$new("x"), Integer$new(3L))
  ))

  # -----
  expect_identical(x, x$body[[1]]$parent)
})


test_that("$append() sets parent on appended Phi nodes", {
  x = BasicBlock$new()
  x$append(Phi$new("x"))

  # -----
  expect_identical(x, x$phi[[1]]$parent)
})


test_that("$append() sets parent on appended body nodes", {
  x = BasicBlock$new()
  x$append(Symbol$new("x"))

  # -----
  expect_identical(x, x$body[[1]]$parent)
})
