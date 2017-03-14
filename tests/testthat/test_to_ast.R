context("to_ast")


test_that("primitives are converted to Primitives", {
  result = to_ast(sum)

  # -----
  expect_is(result, "Primitive")
  #expect_is(result$name, "Symbol")
  pnames = names(formals(args(sum)))
  expect_equal(names(result$params), pnames)

  p1 = result$params[[1]]
  expect_is(p1, "Parameter")
  expect_equal(p1$default, NULL)

  p2 = result$params[[2]]
  expect_is(p2, "Parameter")
  expect_equal(p2$default$value, FALSE)
})


test_that("functions are converted to Functions", {
  result = to_ast(ifelse)

  # -----
  expect_is(result, "Function")
  pnames = names(formals(ifelse))
  expect_equal(names(result$params), pnames)
  
  p1 = result$params[[1]]
  expect_is(p1, "Parameter")
  expect_equal(p1$default, NULL)

  p2 = result$params[[2]]
  expect_is(p2, "Parameter")
  expect_equal(p2$default, NULL)

  p3 = result$params[[3]]
  expect_is(p3, "Parameter")
  expect_equal(p3$default, NULL)
})


test_that("functions with no params are converted to Functions", {
  result = to_ast(function() 42L)

  # -----
  expect_is(result, "Function")
  expect_equal(length(result$params), 0)
})


test_that("function definitions are converted to Functions", {
  code = quote(function(a, b = 3) 42L)

  result = to_ast(code)

  # -----
  expect_is(result, "Function")
  expect_equal(names(result$params), c("a", "b"))

  p1 = result$params[[1]]
  expect_is(p1, "Parameter")
  expect_equal(p1$default, NULL)

  p2 = result$params[[2]]
  expect_is(p2, "Parameter")
  expect_equal(p2$default$value, 3)
})
