context("cran-rcpp")

with_cran_package("tibble", {
  test_that("lookup_rcpp_cran", {
    skip_on_cran()

    res <- lookup_rcpp_cran("matrixToDataFrame", "tibble")
    expect_equal(length(res), 1L)
    expect_match(res[[1]]$content, "List matrixToDataFrame")
  })

  test_that("lookup_rcpp", {
    res <- lookup_rcpp("matrixToDataFrame", "tibble")

    expect_equal(length(res), 1L)
    expect_match(res[[1]]$content, "List matrixToDataFrame")
  })
})
