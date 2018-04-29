context("split_blocks")


test_that("linear blocks are not changed", {
  node = quote_ast({
    x = 3
  })

  result = split_blocks(node$body)

  # -----
  expect_identical(node[[1]], result[[1]][[1]])
})


test_that("linear blocks that end with a flow are not changed", {
  node = quote_ast({
    x = 3
    if (x < 3) {
      x = 3
    }
  })

  result = split_blocks(node$body)

  # -----
  expect_identical(node[[1]], result[[1]][[1]])
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

  result = split_blocks(node$body)

  # -----
  expect_true(length(result) == 3)
  expect_is(result[[1]], "Block")
  expect_is(result[[2]], "Block")
  expect_is(result[[3]], "Block")

  expect_is(result[[1]]$body[[2]], "If")
  expect_is(result[[2]]$body[[2]], "Next")
})
