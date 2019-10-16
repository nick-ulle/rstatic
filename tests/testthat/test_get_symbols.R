context("get_symbols")


test_that("single assignment", {
  exp = quote_ast(x <- y)

  result = get_symbols(exp)
  # -----

  expect_is(result, "SymbolSets")
  expect_named(result, c("defined", "used"))

  expect_length(result$defined, 1L)
  expect_equal(result$defined[[1L]], exp)

  expect_length(result$used, 1L)
  expect_equal(result$used[[1L]], exp$read)
})


test_that("function", {
  exp = quote_ast(function(x, y) {
    ans = x + y
    ans
  })

  result = get_symbols(exp)
  result_recursive = get_symbols(exp, recursive = TRUE)
  # -----

  expect_is(result, "SymbolSets")
  expect_length(result$defined, 0L)
  expect_length(result$used, 0L)

  expect_is(result_recursive, "SymbolSets")
  expect_length(result_recursive$defined, 3L)
  expect_length(result_recursive$used, 4L)
})


test_that("names_used and names_defined", {
  exp = quote_ast({
    x = 0
    for (i in 1:10) {
      x = x + i
    }
  })

  result = get_symbols(exp)
  # -----

  defined = names_defined(result)
  expect_is(defined, "character")
  expect_length(defined, 2L)
  expect_setequal(defined, c("x", "i"))

  used = names_used(result)
  expect_is(used, "character")
  expect_length(used, 4L)
  expect_setequal(used, c(":", "+", "x", "i"))
})
