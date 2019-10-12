context("to_blocks")

test_that("linear code has exit block", {
  node = quote_ast(function() {
      x = 3
      y = 7 + x
  })

  result = to_blocks(node, insert_return = FALSE, ssa = FALSE)

  # -----
  # Check entry block (#2).
  entry = result$body[[2]]
  expect_equal(length(entry), 3)

  # Check exit block (#1).
  exit = result$body[[1]]
  expect_identical(exit$parent, result$body)
  expect_equal(length(exit), 1)
  expect_is(exit[[1]], "Symbol")
  expect_equal(exit[[1]]$value, "._return_")
})


test_that("warning on dead break/next after return", {
  node = quote_ast({
    return (42L)
    next
  })

  # -----
  expect_warning(to_blocks(node, ssa = FALSE), "invalid use")
})


test_that("AST is copied when in_place = FALSE", {
  node = Assignment$new(Symbol$new("x"), Integer$new(42L))

  result = to_blocks(node, in_place = FALSE, ssa = FALSE)

  # -----
  result = result$body[[1]]
  expect_false(identical(result, node))
  expect_false(identical(result$read, node$read))
  expect_false(identical(result$write, node$write))
})


test_that("AST is not copied when in_place = TRUE", {
  node = Assignment$new(Symbol$new("x"), Integer$new(42L))

  result = to_blocks(node, in_place = TRUE, insert_return = FALSE, ssa = FALSE)

  # -----
  result = result[[2]][[1]]
  expect_identical(result, node)
  expect_identical(result$read, node$read)
  expect_identical(result$write, node$write)
})


test_that("nested functions have CFG generated", {
  ast = quote_ast(
    function() {
      x = 1

      f = function(x) {
        x = 21
        x
      }

      f
    })

  result = to_blocks(ast, ssa = FALSE)

  # -----
  fn = result$body[[2]][[2]]$read$body
  expect_is(fn, "BlockList")
})
