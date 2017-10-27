context("collect_crossblock_uses")


test_that("assignment RHSs detected as a crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    y = x
  })
  node = to_cfg(code, ssa = FALSE)
  cfg = node$cfg

  result = collect_crossblock_uses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  expect_equal(sort(uses), c("._return_", "x"))
})


test_that("replacement RHSs detected as crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    y[1] = x
  })
  node = to_cfg(code, ssa = FALSE)
  cfg = node$cfg

  result = collect_crossblock_uses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  expect_equal(sort(uses), c("._return_", "[<-", "x", "y"))
})


test_that("arguments detected as crossblock uses", {
  code = quote({
    if (TRUE)
      x = 3
    else
      x = 4
    mean(x)
  })
  node = to_cfg(code, ssa = FALSE)
  cfg = node$cfg

  result = collect_crossblock_uses(cfg)
  uses = result[[1]]
  assign_blocks = result[[2]]

  # -----
  expect_equal(sort(uses), c("._return_", "mean", "x"))
})
