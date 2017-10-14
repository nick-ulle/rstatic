context("toSSA")


test_that("CFG is copied when inPlace = FALSE", {
  # FIXME:
  #cfg = ControlFlowGraph$new()
  ## Can't compute CFG if entry isn't linked to exit.
  #cfg$add_edge(cfg$entry, cfg$exit)

  #result = toSSA(cfg, inPlace = FALSE)

  ## -----
  #expect_false(identical(cfg, result))
})


test_that("CFG is not copied when inPlace = FALSE", {
  # FIXME:
  #cfg = ControlFlowGraph$new()
  #cfg$add_edge(cfg$entry, cfg$exit)

  #result = toSSA(cfg, inPlace = TRUE)

  ## -----
  #expect_identical(cfg, result)
})


test_that("SSA form for if-statement is correct", {
  ast = toASTq({
    if (x > 3) {
      y = 1
    } else {
      y = 2
    }

    x = y
  })

  node = toCFG(ast)
  cfg = node$cfg

  # -----
  expect_equal(length(cfg), 5)
  y1 = cfg[[3]]$body[[1]]$write
  expect_is(y1, "Symbol")
  expect_equal(y1$ssa_number, 1)

  y2 = cfg[[4]]$body[[1]]$write
  expect_is(y2, "Symbol")
  expect_equal(y2$ssa_number, 2)

  phi = cfg[[5]]$phi[[1]]
  expect_is(phi, "Phi")
  expect_is(phi$write, "Symbol")
  expect_equal(phi$write$ssa_number, 3)

  args = phi$read
  expect_named(args, c("%3", "%4"), ignore.order = TRUE)
  expect_equal(args[["%3"]]$ssa_number, 1)
  expect_equal(args[["%4"]]$ssa_number, 2)
})


test_that("Phi nodes placed for assign in if in loop", {
  ast = toASTq({
    x = 0

    for (i in 2:10) {
      if (x > 1)
        # RHS should come from a Phi
        x = x + 1
    }
  })

  node = toCFG(ast)
  cfg = node$cfg

  # -----
  expect_true(has_phi(cfg[["%3"]], "x"))
})

test_that("uses of global variables are recorded", {
  result = toCFGq(
    function(x, y) {
      y = z + 1
      x = 3
      x + y + a
    }
  )

  # -----
  expect_length(result$global_uses, 3)
  expect_true("+" %in% result$global_uses)
  expect_true("z" %in% result$global_uses)
  expect_true("a" %in% result$global_uses)
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
#  ast = toAST(ifib)
#  cfg = toCFG(ast)
#
#  # ----
#  browser()
})
