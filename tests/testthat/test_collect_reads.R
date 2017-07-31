context("collectReads")


test_that("collecting reads on literals returns empty vector", {
  code = Integer$new(5)

  result = collectReads(code)

  # -----
  expect_equal(length(result), 0)
  expect_is(result, "character")
})


test_that("collecting reads on x returns x", {
  code = Symbol$new("x")

  result = collectReads(code)

  # -----
  expect_equal(result, "x")
})


test_that("collecting reads on mean(x) returns x", {
  code = Call$new("mean", list(Symbol$new("x")))
  
  result = collectReads(code)

  # -----
  expect_equal(result, "x")
})


test_that("collecting reads on sum(x, 5, y, 7, x) returns x, y", {
  code = Call$new("sum", list(
      Symbol$new("x"), Integer$new(5), Symbol$new("y"), Integer$new(7),
      Symbol$new("x")
  ))

  result = collectReads(code)

  # -----
  expect_true("x" %in% result)
  expect_true("y" %in% result)
  expect_equal(length(result), 2)
})


test_that("collecting reads on x = y returns y", {
  code = Assign$new(Symbol$new("x"), Symbol$new("y"))

  result = collectReads(code)

  # -----
  expect_equal(result, "y")
})


test_that("collecting reads on return(x) returns x", {
  code = Return$new(Symbol$new("x"))

  result = collectReads(code)

  # -----
  expect_equal(result, "x")
})


test_that("collecting reads on x[i] = y returns i, y", {
  code = Replacement$new(
    Symbol$new("x"),
    "[<-",
    list(Symbol$new("i"), Symbol$new("y"))
  )

  result = collectReads(code)

  # -----
  expect_true("i" %in% result)
  expect_true("y" %in% result)
  expect_equal(length(result), 2)
})


test_that("collecting reads on {x} returns x", {
  code = Brace$new(list(Symbol$new("x")))

  result = collectReads(code)

  # -----
  expect_equal(result, "x")
})
