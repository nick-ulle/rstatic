context("to_cfg")


test_that("AST is copied when in_place = FALSE", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  cfg = to_cfg(ast, in_place = FALSE)

  # -----
  node = cfg[[1]]$body[[1]]
  expect_false(identical(ast, node))
  expect_false(identical(ast$read, node$read))
  expect_false(identical(ast$write, node$write))
})


test_that("AST is not copied when in_place = TRUE", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  cfg = to_cfg(ast, in_place = TRUE)

  # -----
  node = cfg[[1]]$body[[1]]
  expect_identical(ast, node)
  expect_identical(ast$read, node$read)
  expect_identical(ast$write, node$write)
})


test_that("nodes are reparented to containing BasicBlock", {
  ast = Assign$new(Symbol$new("x"), Integer$new(42L))

  cfg = to_cfg(ast)

  # -----
  expect_identical(cfg[[1]], cfg[[1]]$body[[1]]$parent)
})
