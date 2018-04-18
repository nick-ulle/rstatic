context("to_data_frame")

test_that("", {
  #exp = quote_cfg({
  #  x = rnorm(1e6)
  #  if (x[[1]] < 1) {
  #    y = x[1:5]
  #  } else {
  #    y = rep(0, 5)
  #  }
  #}, ssa = FALSE)
  exp = quote_cfg({
    w = rnorm(1) > 1
    x = 4
    if (w)
      #z = w
      z = x
    else
      z = x^2
  }, ssa = FALSE)

  #live = live_variables(exp$cfg)
  code = to_data_frame(exp$cfg)
  result = backward_traversal(code, live_vars)
  code$live_exit = result
  code$live_entry = Map(live_vars, code$line, code$live_exit)

  analyze = function(pc, code, ...) {
    # Can this analysis figure out whether to insert an rm?
    # Or can the propagate function?
    #
    # The equations are:
    #   after  = exit - entry     <- both available here
    #   before = fwd - entry      <- only fwd available in propagate
    code[[pc, "live_exit"]]
  }

  propagate = function(old, new) {
    new
  }

  z = forward_traversal(code, analyze, propagate)
  code$live_fwd = z

  # Get lines to insert rm after/before.
  code$after = Map(setdiff, code$live_entry, code$live_exit)
  code$before = Map(setdiff, code$live_fwd, code$live_entry)

  #x = Symbol$new("x")
  #code2 = insert_after(code, 3, x)
  browser()
})
