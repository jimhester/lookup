context("github-rcpp")

with_github_package("tidyverse/tibble", {
  test_that("lookup_github_cran", {
    skip_on_cran()

    res <- lookup_rcpp_github("matrixToDataFrame", "tidyverse", "tibble")
    expect_equal(length(res), 1L)
    expect_match(res[[1]]$content, "List matrixToDataFrame")
  })

  test_that("lookup_rcpp", {
    lookup_rcpp("matrixToDataFrame", "tibble")

    expect_equal(length(res), 1L)
    expect_match(res[[1]]$content, "List matrixToDataFrame")
  })
})
