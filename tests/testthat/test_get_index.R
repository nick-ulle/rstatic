context("get_index")

test_that("[ with simple indexes", {
  node = quote_ast(y[a, b])

  result = get_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
  expect_identical(result[[2L]], node$args$contents[[3L]])
})


test_that("[ with empty index", {
  node = quote_ast(y[a, ])

  result = get_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
  expect_identical(result[[2L]], node$args$contents[[3L]])
})


test_that("[ with `drop` argument", {
  node = quote_ast(y[a, b, drop = FALSE])

  result = get_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
  expect_identical(result[[2L]], node$args$contents[[3L]])
})


test_that("[[ with `exact` argument", {
  node = quote_ast(y[[a, exact = TRUE]])

  result = get_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 1L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
})


test_that("[<- ", {
  node = quote_ast(y[a, b] <- z)

  result = get_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$read$args$contents[[2L]])
  expect_identical(result[[2L]], node$read$args$contents[[3L]])
})
