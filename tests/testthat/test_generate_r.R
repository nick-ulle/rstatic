context("generate_r")


test_that("no branches", {
  r_code = quote({
    x = 1
    3
  })
  
  code = to_cfg(r_code, ssa = FALSE)
  result = rebuild_r(code$cfg)

  # -----
  # NOTE: Code is deparsed to a string here because `==` does not compare code
  # inside of curly braces (and possibly other language objects).
  r_str = rstatic:::deparse_to_string(r_code)
  str = rstatic:::deparse_to_string(result)

  expect_equal(r_str, str)
})


test_that("if-statement (depth 1)", {
  r_code = quote({
    x = 1
    if (x > 4) {
      x = 2
    } else {
      x = 3
    }
    3
  })

  code = to_cfg(r_code, ssa = FALSE)
  result = rebuild_r(code$cfg)

  # -----
  r_str = rstatic:::deparse_to_string(r_code)
  str = rstatic:::deparse_to_string(result)

  expect_equal(r_str, str)
})


test_that("for-loop (depth 1)", {
  r_code = quote({
    x = 0
    for (i in 1:10) {
      x = x + i
    }
    x = x^2
  })

  code = to_cfg(r_code, ssa = FALSE)

  result = rebuild_r(code$cfg, NULL)

  # -----
  r_str = rstatic:::deparse_to_string(r_code)
  str = rstatic:::deparse_to_string(result)

  expect_equal(r_str, str)
})


test_that("if-statement (depth 2)", {
  r_code = quote({
    if (TRUE) {
      if (1) {
        x = 1
      } else {
        x = 2
      }
    } else {
      if (2) {
        x = 3
      } else {
        x = 4
      }
    }
    x = 5
  })

  code = to_cfg(ast, ssa = FALSE)
  result = rebuild_r(code$cfg, NULL)

  # -----
  r_str = rstatic:::deparse_to_string(r_code)
  str = rstatic:::deparse_to_string(result)

  expect_equal(r_str, str)
})


test_that("if-statement (depth 1) with early return", {
  r_code = quote({
    if (x > 1) {
      return (x)
    } else {
      x = x + 1
      x = x^2
    }
    x + 7
  })

  code = to_cfg(r_code, ssa = FALSE)
  result = rebuild_r(code$cfg, NULL)

  # -----
  r_str = rstatic:::deparse_to_string(r_code)
  str = rstatic:::deparse_to_string(result)

  expect_equal(r_str, str)
})


test_that("if-statement (depth 2) with early return", {
  r_code = quote({
    if (x == 42) {
      return (x)
    } else {
      if (x > 12)
        #x = sqrt(x)
        return (x)
      else
        x = x^2
      x = x + 1
    }
    x = x + 7
  })

  code = to_cfg(r_code, ssa = FALSE)
  result = rebuild_r(code$cfg, NULL)

  # -----
  r_str = rstatic:::deparse_to_string(r_code)
  str = rstatic:::deparse_to_string(result)

  expect_equal(r_str, str)
})
