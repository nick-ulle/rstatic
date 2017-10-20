
context("collapseNamespaces")

test_that("namespaces are collapsed (inPlace = FALSE)", {
  node = toASTq( rstatic::foo )

  result = collapseNamespaces(node)

  # -----
  expect_is(result, "Symbol")
  expect_equal(result$namespace, "rstatic")
  expect_equal(result$namespace_fn$name, "::")
  expect_equal(result$basename, "foo")

  expect_false(identical(result, node))
})


test_that("namespaces are collapsed (inPlace = TRUE)", {
  node = toASTq( x <- rstatic::foo )

  result = collapseNamespaces(node, inPlace = TRUE)

  # -----
  expect_true(identical(node$read, result$read))

  result = result$read
  expect_equal(result$namespace, "rstatic")
  expect_equal(result$namespace_fn$name, "::")
  expect_equal(result$basename, "foo")
  expect_identical(result$parent, node)
})
