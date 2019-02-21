context("find_nodes")

test_that("finding Symbols", {
  skip("")
  code = quote_ast({
    x = 3
    y = 3 + x
    z = sum(y, x, x)
    return (x + z)
  })

  result = find_nodes(code, function(node) {
    is(node, "Symbol") && node$value == "x"
  })

  # -----
  expect_length(result, 5)
  expect_equal(result[[1]], c(1, 1))
  expect_equal(result[[2]], c(2, 2, 2, 2))
  expect_equal(result[[3]], c(3, 2, 2, 2))
  expect_equal(result[[4]], c(3, 2, 2, 3))
  expect_equal(result[[5]], c(4, 2, 2, 1))
})




context("replace_nodes") # ----------------------------------------

test_that("renaming Symbols", {
  x1 = Symbol$new("x")
  x2 = Symbol$new("x")
  y = Symbol$new("y")
  div = Call$new("/", y, x2)

  code = Call$new("sum", x1, div, Symbol$new("z"))

  rename_symbols = function(node, name, newname)
  { # Rename symbols to something else.
    if (is(node, "Symbol") && node$value == name)
      node$value = newname

    node
  }

  new_name = "newx"
  replace_nodes(code, rename_symbols, "x", new_name, in_place = TRUE)

  # -----
  expect_equal(x1$value, new_name)
  expect_equal(x2$value, new_name)
  expect_equal(y$value, "y")
})
