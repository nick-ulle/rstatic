context("Brace")


test_that("$new() sets parents on body nodes", {
  x = Brace$new(body = list(
    Assign$new(Symbol$new("x"), Integer$new(3L))
  ))

  # -----
  expect_identical(x, x$body[[1]]$parent)
})


test_that("$append_phi() sets parent on appended Phi nodes", {
  x = Brace$new()
  x$append_phi(Phi$new("x"))

  # -----
  expect_identical(x, x$phi[[1]]$parent)
})


test_that("copying sets parents on body nodes", {
  x = Brace$new(body = list(
    Assign$new(Symbol$new("x"), Integer$new(3L))
    , Assign$new(Symbol$new("x"), Symbol$new("y"))
  ))

  y = x$copy()

  # -----
  expect_identical(y, y$body[[1]]$parent)
  expect_identical(y, y$body[[2]]$parent)

  expect_false(identical(x, y$body[[1]]$parent))
  expect_false(identical(y, x$body[[1]]$parent))
})


test_that("copying sets parents on Phi nodes", {
  x = Brace$new()
  x$append_phi(Phi$new("x"))

  y = x$copy()

  # -----
  expect_identical(y, y$phi[[1]]$parent)

  expect_false(identical(x, y$phi[[1]]$parent))
  expect_false(identical(y, x$phi[[1]]$parent))
})
