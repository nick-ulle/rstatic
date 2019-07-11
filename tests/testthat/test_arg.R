context("arg_ functions")

test_that("object for [", {
  node = quote_ast(y[1, 2])

  result = arg_object(node)

  # -----
  expect_identical(result, node$args$contents[[1L]])
})


test_that("indexes for [", {
  node = quote_ast(y[a, b])

  result = arg_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
  expect_identical(result[[2L]], node$args$contents[[3L]])
})


test_that("indexes for [ with empty index", {
  node = quote_ast(y[a, ])

  result = arg_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
  expect_identical(result[[2L]], node$args$contents[[3L]])
})


test_that("indexes for [ with `drop` argument", {
  node = quote_ast(y[a, b, drop = FALSE])

  result = arg_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
  expect_identical(result[[2L]], node$args$contents[[3L]])
})


test_that("indexes for [[ with `exact` argument", {
  node = quote_ast(y[[a, exact = TRUE]])

  result = arg_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 1L)
  expect_identical(result[[1L]], node$args$contents[[2L]])
})


test_that("indexes for [<-", {
  node = quote_ast(y[a, b] <- z)

  result = arg_index(node)

  # -----
  expect_is(result, "list")
  expect_length(result, 2L)
  expect_identical(result[[1L]], node$read$args$contents[[2L]])
  expect_identical(result[[2L]], node$read$args$contents[[3L]])
})


test_that("value for [<-", {
  node = quote_ast(y[1, b] <- z)
  
  result = arg_value(node)

  # -----
  expect_identical(result, node$read$args$contents[[4L]])
})
