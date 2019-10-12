context("Brace")


test_that("$new() sets parents on contents", {
  x = Brace$new(contents = list(
    Assignment$new(Symbol$new("x"), Integer$new(3L))
  ))

  # -----
  expect_identical(x, x$contents[[1]]$parent)
})


test_that("copying sets parents on contents", {
  x = Brace$new(list(
    Assignment$new(Symbol$new("x"), Integer$new(3L))
    , Assignment$new(Symbol$new("x"), Symbol$new("y"))
  ))

  y = copy(x)

  # -----
  expect_identical(y, y$contents[[1]]$parent)
  expect_identical(y, y$contents[[2]]$parent)

  expect_false(identical(x, y$contents[[1]]$parent))
  expect_false(identical(y, x$contents[[1]]$parent))
})


#test_that("$set_phi() sets parent on Phi nodes", {
#  x = Brace$new()
#  x$set_phi(Phi$new("x"))
#
#  # -----
#  expect_identical(x, x$phi[[1]]$parent)
#})


#test_that("copying sets parents on Phi nodes", {
#  x = Brace$new()
#  x$set_phi(Phi$new("x"))
#
#  y = copy(x)
#
#  # -----
#  expect_identical(y, y$phi[[1]]$parent)
#
#  expect_false(identical(x, y$phi[[1]]$parent))
#  expect_false(identical(y, x$phi[[1]]$parent))
#})
