context("cran-rcpp")

test_that("lookup_rcpp_cran", {
  skip_on_cran()

  res <- lookup_rcpp_cran("node_type", "xml2")
  expect_equal(length(res), 1L)
  expect_equal(res[[1]]$content, "int node_type(XPtrNode node) {\n  return node->type;\n}")
})
