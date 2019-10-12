context("insert_return")

test_that("return inserted for Literal", {
  ast = quote_ast(3.14)

  result = insert_return(ast)

  # -----
  expect_is(result, "Return")
  expect_is(result$read, "Numeric")
})


test_that("return inserted for Symbol", {
  ast = quote_ast(x)

  result = insert_return(ast)

  # -----
  expect_is(result, "Return")
  expect_is(result$read, "Symbol")
})


test_that("return inserted for Call", {
  ast = quote_ast(sum(x, 1, 3))

  result = insert_return(ast)

  # -----
  expect_is(result, "Return")
  expect_is(result$read, "Call")
})


test_that("return inserted for Assignment", {
  ast = quote_ast(x <- 3)

  result = insert_return(ast)

  # -----
  expect_is(result, "list")
  expect_length(result, 2)

  expect_is(result[[1]], "Assignment")
  expect_is(result[[2]], "Return")
})


test_that("return inserted after While, without duplicate Brace", {
  ast = quote_ast({
    while (x < 10) x = x + 1
  })

  result = insert_return(ast)

  # -----
  expect_is(result, "Brace")

  expect_is(result$contents[[1]], "While")
  expect_identical(result$contents[[1]]$parent, result)

  expect_is(result$contents[[2]], "Return")
  expect_identical(result$contents[[2]]$parent, result)
})


test_that("return inserted after While, adding Brace", {
  ast = quote_ast(
    while (x < 10) x = x + 1
  )

  result = insert_return(ast)

  # -----
  expect_is(result, "list")

  expect_is(result[[1]], "While")
  expect_is(result[[2]], "Return")
})


test_that("return inserted for Function", {
  f = function(x) 42L

  ast = to_ast(f)

  result = insert_return(ast)

  # -----
  expect_is(result, "Function")
  brace = result$body
  expect_is(brace$contents[[1]], "Return")
  expect_identical(brace$contents[[1]]$parent, brace)
})
