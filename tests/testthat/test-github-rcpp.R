context("github-rcpp")
with_github_package("tidyverse/tibble", {
  test_that("fetch_symbol_map", {
    s <- as.source_type("tibble", "rcpp")
    s <- fetch_symbol_map(s)
    expect_true(length(s$map_lines) > 0)
  })

  test_that("parse_symbol_map", {
    s <- as.source_type("tibble", "rcpp")
    s <- fetch_symbol_map(s)

    s <- parse_symbol_map(s)
    expect_true(length(s$map) > 0)
  })
})
  #test_that("lookup_github_cran", {
    #skip_on_cran()

    #res <- lookup_rcpp_github("matrixToDataFrame", "tidyverse", "tibble")
    #expect_equal(length(res), 1L)
    #expect_match(res[[1]]$content, "List matrixToDataFrame")
  #})

  #test_that("lookup_rcpp", {
    #res <- lookup_rcpp("matrixToDataFrame", "tibble")

    #expect_equal(length(res), 1L)
    #expect_match(res[[1]]$content, "List matrixToDataFrame")
  #})

  #test_that("lookup_rcpp", {
    #res <- lookup_rcpp("matrixToDataFrame2", "tibble")

    #expect_identical(length(res), NULL)
  #})
#})
