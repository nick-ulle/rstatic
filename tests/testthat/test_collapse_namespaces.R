
context("collapse_namespaces")

test_that("namespaces are collapsed (in_place = FALSE)", {
  node = quote_ast( rstatic::foo )

  result = collapse_namespaces(node)

  # -----
  expect_is(result, "Symbol")
  expect_equal(result$namespace, "rstatic")
  expect_equal(result$namespace_fn$name, "::")
  expect_equal(result$basename, "foo")

  expect_false(identical(result, node))
})


test_that("namespaces are collapsed (in_place = TRUE)", {
  node = quote_ast( x <- rstatic::foo )

  result = collapse_namespaces(node, in_place = TRUE)

  # -----
  expect_true(identical(node$read, result$read))

  result = result$read
  expect_equal(result$namespace, "rstatic")
  expect_equal(result$namespace_fn$name, "::")
  expect_equal(result$basename, "foo")
  expect_identical(result$parent, node)
})
