context("equal")


test_that("Literals", {
  # TRUE -----
  result = Integer$new(1) == Integer$new(1)
  expect_true(result)

  result = Character$new("hi") == Character$new("hi")
  expect_true(result)


  # FALSE -----
  result = Character$new("bye") == Character$new("hi")
  expect_false(result)

  result = Character$new("yby") == Numeric$new(3)
  expect_false(result)
})


test_that("Symbols", {
  # TRUE -----
  result = Symbol$new("x") == Symbol$new("x")
  expect_true(result)

  # FALSE -----
  result = Symbol$new("x", 1) == Symbol$new("x", 2)
  expect_false(result)

  result = Parameter$new("x", default = Integer$new(1)) ==
    Parameter$new("x", default = Integer$new(2))
  expect_false(result)

  # TODO: Namespaces
})


test_that("Assignments", {
  # TRUE -----
  result = Assign$new(Symbol$new("y"), Symbol$new("hello")) ==
    Assign$new(Symbol$new("y"), Symbol$new("hello"))
  expect_true(result)

  # FALSE -----
  result = Assign$new(Symbol$new("x"), 3) == Assign$new(Symbol$new("x"), 4)
  expect_false(result)

  result = Assign$new(Symbol$new("z"), 3) == Assign$new(Symbol$new("x"), 3)
  expect_false(result)
})


test_that("Calls", {
  # TRUE -----
  c1 = Call$new("f")
  c2 = Call$new("f")
  result = c1 == c2
  expect_true(result)

  c1 = Call$new("f", Symbol$new("x"))
  c2 = Call$new("f", Symbol$new("x"))
  result = c1 == c2
  expect_true(result)

  # FALSE -----
  c1 = Call$new("f", Symbol$new("x"))
  c2 = Call$new("f", 3)
  result = c1 == c2
  expect_false(result)

  c1 = Call$new("g", Symbol$new("x"))
  c2 = Call$new("f", Symbol$new("x"))
  result = c1 == c2
  expect_false(result)
})
