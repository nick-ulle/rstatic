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
  # Get the relevant lines.
  cfg = node$cfg
  if_statement = cfg[[cfg$entry]]$body[[1]]
  true_block = cfg[[if_statement$true]]
  false_block = cfg[[if_statement$false]]

  y_true = true_block[[1]]$write
  expect_false(is.na(y_true$ssa_number))

  y_false = false_block[[1]]$write
  expect_false(is.na(y_false$ssa_number))

  phi = cfg[["%1"]]$phi[["y"]]
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
  cfg = node$cfg
  entry_block = cfg[[cfg$entry]]

  expect_false( is.na(entry_block$body[[1]]$write$ssa_number) )

  # TODO: Test the SSA more thoroughly.
  #loop = node$body[[2]]
  #expect_length(loop$test$phi, 2)

  #counter_phi = loop$test$phi[[1]]
  #expect_length(counter_phi$read, 3)

  x_phi = cfg[["%1"]]$phi[["x"]]
  expect_length(x_phi$read, 3)
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
