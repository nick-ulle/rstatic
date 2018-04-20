context("node_apply")


test_that("Missing arguments", {

    node = quote_ast(x[1, "column"])
    node$args[[2]]
    node_apply(node, print)

    node = quote_ast(x[, "column"])
    node$args[[2]]
    node_apply(node, print)

})
