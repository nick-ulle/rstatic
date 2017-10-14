context("collectCrossblockUses")


test_that("assignment RHSs detected as a crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    y = x
  })
  node = toCFG(code, ssa = FALSE)
  cfg = node$cfg

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
  node = toCFG(code, ssa = FALSE)
  cfg = node$cfg

  result = collectCrossblockUses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  expect_equal(uses, c("._return_", "[<-", "y", "x"))
})


test_that("arguments detected as crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    mean(x)
  })
  node = toCFG(code, ssa = FALSE)
  cfg = node$cfg

  result = collectCrossblockUses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  expect_equal(uses, c("._return_", "mean", "x"))
})
