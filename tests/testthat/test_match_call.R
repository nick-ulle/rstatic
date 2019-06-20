context("match_call")

test_that("arguments matched for global function", {
  exp = quote(rnorm(mean = 0, s = 3, 1))

  result = match_call(exp)

  # -----
  expect_is(result, "call")
  expect_named(result, c("", "n", "mean", "sd"))
  expect_identical(result[[1L]], as.symbol("rnorm"))
  expect_identical(result[[2L]], 1)
  expect_identical(result[[3L]], 0)
  expect_identical(result[[4L]], 3)

  expect_identical(match_call(exp), match.call(rnorm, exp))
})


test_that("arguments matched for primitive", {
  exp = quote(log(base = 2, 10))

  result = match_call(exp)

  # -----
  expect_is(result, "call")
  expect_named(result, c("", "x", "base"))
  expect_identical(result[[2L]], 10)
  expect_identical(result[[3L]], 2)
})

test_that("arguments matched for custom function", {
  exp = quote_ast(mean(1:3))

  mean = function(value) NULL
  result1 = match_call(exp, mean)

  e = new.env(parent = emptyenv())
  e[["mean"]] = function(xs) NULL
  result2 = match_call(exp, envir = e)

  # -----
  expect_is(result1, "Call")
  expect_named(result1$args, c("value"))

  expect_is(result2, "Call")
  expect_named(result2$args, c("xs"))
})

test_that("arguments matched for anonymous function", {
  exp = quote_ast((function(x) x)(3))

  result = match_call(exp)

  # -----
  expect_is(result, "Call")
  expect_named(result$args, c("x"))
})

