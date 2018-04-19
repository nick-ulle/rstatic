context("node_apply")


test_that("Missing arguments", {

    node = quote_ast(x[, "column"])
    node_apply(node, print)

})
