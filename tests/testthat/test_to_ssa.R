context("to_ssa")


test_that("SSA form for if-statement", {
  node = quote_ast({
    x = 1
    if (x > 3) {
      y = 1
    } else {
      y = 2
    }

    x = y
  })

  result = to_blocks(node, ssa = TRUE)

  # -----
  if_statement = result[[2]]$contents[[2]]
  true = result[[if_statement$true$name]]
  false = result[[if_statement$false$name]]

  y_true = true$contents[[1]]$write
  expect_false(is.na(y_true$ssa_number))

  y_false = false$contents[[1]]$write
  expect_false(is.na(y_false$ssa_number))

  phi = result[[3]]$phi[["y"]]
  expect_is(phi, "Phi")
  expect_is(phi$write, "Symbol")
  expect_false(is.na(phi$write$ssa_number))
  #expect_equal(phi$write$ssa_number, 3)

  expect_true(all(phi$ids %in% c(true$id, false$id)))
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

  result = to_blocks(node, ssa = TRUE)

  # -----
  entry_block = result[[2]]

  expect_false( is.na(entry_block$contents[[1]]$write$ssa_number) )

  # TODO: Test the SSA more thoroughly.
  #loop = node$body[[2]]
  #expect_length(loop$test$phi, 2)

  #counter_phi = loop$test$phi[[1]]
  #expect_length(counter_phi$read, 3)

  x_phi = result[[3]]$phi[["x"]]
  expect_length(x_phi$contents, 3)
  expect_length(x_phi$ids, 3)
})

test_that("SSA form for parameters", {
  node = quote_ast(
    function(x, y) {
      y = z + x
      x = 3
      x + y + a
    }
  )

  result = to_blocks(node, ssa = TRUE)

  # -----
  missing_ssa = vapply(result$params, function(p) is.na(p$ssa_number), NA)
  expect_false(any(missing_ssa))
})


test_that("ifib", {
  ifib = function(n = 0L) {
    if (n == 0L)
      return (0L)
    #else if (n == 1L)
    #  return (1L)

    old = 0L
    new = 1L
    n = n - 1L
    for (i in 1:n) {
      old_new = new
      new = old + old_new
      old = old_new
    }

    return (new)
  }

  node = to_ast(ifib)
  node = to_blocks(node)

  # ----
})
