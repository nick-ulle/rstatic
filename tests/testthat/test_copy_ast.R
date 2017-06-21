context("ASTNode$copy")


test_that("copying with inherited copy method sets correct parents", {
  x = Assign$new(Symbol$new("x"), Complex$new(1+4i))

  y = x$copy()

  # -----
  expect_identical(x, x$read$parent)
  expect_identical(x, x$write$parent)
  expect_identical(y, y$write$parent)
  expect_identical(y, y$read$parent)

  # Make sure x is not parent to y's children and vice-versa.
  expect_false(identical(x, y$write$parent))
  expect_false(identical(y, x$write$parent))
})


test_that("copying sets correct parents on grandchildren", {
  x = Assign$new(Symbol$new("x"), 
    Call$new("+", list(Integer$new(2), Integer$new(3)))
  )
  
  y = x$copy()

  # -----
  expect_identical(x$read, x$read$args[[1]]$parent)
  expect_identical(x$read, x$read$args[[2]]$parent)
  expect_identical(y$read, y$read$args[[1]]$parent)
  expect_identical(y$read, y$read$args[[2]]$parent)

  expect_false(identical(x$read, y$read$args[[1]]$parent))
  expect_false(identical(y$read, x$read$args[[1]]$parent))
})


test_that("copying a Brace sets correct parents", {
  x = Brace$new(list(
      Assign$new(Symbol$new("x"), Integer$new(5))
      , Character$new("Hi")
  ))

  y = x$copy()

  # -----
  expect_identical(x, x$body[[1]]$parent)
  expect_identical(x, x$body[[2]]$parent)
  expect_identical(y, y$body[[1]]$parent)
  expect_identical(y, y$body[[2]]$parent)

  expect_false(identical(x, y$body[[1]]$parent))
  expect_false(identical(y, x$body[[1]]$parent))
})


test_that("copying a Application sets correct parents", {
  x = Call$new("*", list(Symbol$new("x"), Numeric$new(4.2)))

  y = x$copy()

  # -----
  expect_identical(x, x$args[[1]]$parent)
  expect_identical(x, x$args[[2]]$parent)
  expect_identical(y, y$args[[1]]$parent)
  expect_identical(y, y$args[[2]]$parent)

  expect_false(identical(x, y$args[[1]]$parent))
  expect_false(identical(y, x$args[[1]]$parent))
})


test_that("copying a Callable sets correct parents", {
  x = Function$new(
    list(Parameter$new("x"), Parameter$new("y", Numeric$new(3.14)))
    , Integer$new(8L)
  )

  y = x$copy()

  # -----
  expect_identical(x, x$params[[1]]$parent)
  expect_identical(x, x$params[[2]]$parent)
  expect_identical(y, y$params[[1]]$parent)
  expect_identical(y, y$params[[2]]$parent)

  expect_false(identical(x, y$params[[1]]$parent))
  expect_false(identical(y, x$params[[1]]$parent))
})
