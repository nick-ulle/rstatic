
context("collapse_namespaces")

test_that("namespaces are collapsed (in_place = FALSE)", {
  node = quote_ast( rstatic::foo )

  result = replace_nodes(node, collapse_namespace)

  # -----
  expect_is(result, "Symbol")
  expect_equal(result$namespace, "rstatic")
  expect_equal(result$namespace_fn$ssa_name, "::")
  expect_equal(result$value, "foo")

  expect_false(identical(result, node))
})


test_that("namespaces are collapsed (in_place = TRUE)", {
  node = quote_ast( x <- rstatic::foo )

  result = replace_nodes(node, collapse_namespace, in_place = TRUE)

  # -----
  expect_true(identical(node$read, result$read))

  result = result$read
  expect_equal(result$namespace, "rstatic")
  expect_equal(result$namespace_fn$ssa_name, "::")
  expect_equal(result$value, "foo")
  expect_identical(result$parent, node)
})
