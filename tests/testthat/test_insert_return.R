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


test_that("return inserted for Assign", {
  ast = quote_ast(x <- 3)

  result = insert_return(ast)

  # -----
  expect_is(result, "list")
  expect_length(result, 2)

  expect_is(result[[1]], "Assign")
  expect_is(result[[2]], "Return")
})


test_that("return inserted after While, without duplicate Brace", {
  ast = quote_ast({
    while (x < 10) x = x + 1
  })

  result = insert_return(ast)

  # -----
  expect_is(result, "Brace")

  expect_is(result$body[[1]], "While")
  expect_identical(result$body[[1]]$parent, result)

  expect_is(result$body[[2]], "Return")
  expect_identical(result$body[[2]]$parent, result)
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
  expect_is(result$body[[1]], "Return")
  expect_identical(result$body[[1]]$parent, result$body)
})
