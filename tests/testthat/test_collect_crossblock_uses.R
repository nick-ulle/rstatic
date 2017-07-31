context("collectCrossblockUses")


test_that("assignment RHSs detected as a crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    y = x
  })
  cfg = toCFG(code, ssa = FALSE)

  result = collectCrossblockUses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  expect_equal(uses, c("._return_", "x"))
})


test_that("replacement RHSs detected as crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    y[1] = x
  })
  cfg = toCFG(code, ssa = FALSE)

  result = collectCrossblockUses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  # FIXME: Should y really be a use here?
  expect_equal(uses, c("._return_", "y", "x"))
})


test_that("arguments detected as crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    mean(x)
  })
  cfg = toCFG(code, ssa = FALSE)

  result = collectCrossblockUses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  expect_equal(uses, c("._return_", "x"))
})
