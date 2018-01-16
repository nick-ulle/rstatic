context("to_ssa")


test_that("SSA form for if-statement is correct", {
  node = quote_ast({
    if (x > 3) {
      y = 1
    } else {
      y = 2
    }

    x = y
  })

  node = to_cfg(node, ssa = FALSE)
  rstatic:::to_ssa(node)

  # -----
  true_block = node$body[[1]][[1]]$true
  false_block = node$body[[1]][[1]]$false

  y_true = true_block[[1]]$write
  expect_false(is.na(y_true$ssa_number))

  y_false = false_block[[1]]$write
  expect_false(is.na(y_false$ssa_number))

  phi = node$body[[2]]$phi[[1]]
  expect_is(phi, "Phi")
  expect_is(phi$write, "Symbol")
  expect_equal(phi$write$ssa_number, 3)

  args = phi$read
  expect_named(args, c(true_block$id, false_block$id), ignore.order = TRUE)
})


test_that("Phi nodes placed for Assign in if-statement in for-loop", {
  node = quote_ast({
    x = 0

    for (i in 2:10) {
      if (x > 1)
        # RHS should come from a Phi
        x = x + 1
    }
  })

  node = to_cfg(node, ssa = FALSE)
  rstatic:::to_ssa(node)

  # -----
  expect_false(is.na(node$body[[1]][[1]]$write$ssa_number))

  # FIXME: Test the SSA more thoroughly.
  loop = node$body[[1]][[2]]
  expect_length(loop$test$phi, 2)

  counter_phi = loop$test$phi[[1]]
  expect_length(counter_phi$read, 3)

  x_phi = loop$test$phi[[2]]
  expect_length(counter_phi$read, 3)
})

test_that("uses of global variables are recorded", {
  node = quote_ast(
    function(x, y) {
      y = z + 1
      x = 3
      x + y + a
    }
  )

  node = to_cfg(node)
  rstatic:::to_ssa(node)

  # -----
  expect_length(node$ssa$global_uses, 3)
  expect_true("+" %in% node$ssa$global_uses)
  expect_true("z" %in% node$ssa$global_uses)
  expect_true("a" %in% node$ssa$global_uses)
})


test_that("ifib", {
#  ifib = function(n = 0L) {
#    if (n == 0L)
#      return (0L)
#    #else if (n == 1L)
#    #  return (1L)
#
#    old = 0L
#    new = 1L
#    n = n - 1L
#    for (i in 1:n) {
#      old_new = new
#      new = old + old_new
#      old = old_new
#    }
#
#    return (new)
#  }
#
#  node = to_ast(ifib)
#  node = to_cfg(node)
#  to_ssa(node, in_place = TRUE)
#
#  # ----
})
