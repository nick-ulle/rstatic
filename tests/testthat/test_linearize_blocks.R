context("linearize_blocks")


test_that("linear blocks are not changed", {
  node = quote_ast({
    x = 3
  })

  result = linearize_blocks(node)

  # -----
  expect_identical(node, result)
})


test_that("linear blocks that end with a flow are not changed", {
  node = quote_ast({
    x = 3
    if (x < 3) {
      x = 3
    }
  })

  result = linearize_blocks(node)

  # -----
  expect_identical(node, result)
})


test_that("non-linear blocks are split (depth 1)", {
  node = quote_ast({
    x = 3
    if (x > 2) {
      x = x + 1
    }
    y = x
    next
    z = y
  })

  result = linearize_blocks(node)

  # -----
  expect_length(result, 3)
  expect_is(result[[1]], "Brace")
  expect_is(result[[2]], "Brace")
  expect_is(result[[3]], "Brace")

  expect_is(result[[1]]$body[[2]], "If")
  expect_is(result[[2]]$body[[2]], "Next")
})


test_that("non-linear blocks are split (depth 2)", {
  node = quote_ast({
    x = 3
    if (x > 2) {
      x = x + 1
      break
      x = 13
    }
  })

  result = linearize_blocks(node)

  # -----
  if_node = result$body[[2]]
  expect_length(if_node$true, 2)
  expect_is(if_node$true[[1]], "Brace")
  expect_is(if_node$true[[2]], "Brace")
  expect_identical(if_node, if_node$true[[1]]$parent)
  expect_identical(if_node, if_node$true[[2]]$parent)
})

