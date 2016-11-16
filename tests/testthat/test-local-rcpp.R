with_local_package("TestRcpp", {
  context("local-rcpp")
  test_that("rcpp_symbol_map", {

    map <- rcpp_symbol_map_local("TestRcpp")

    expect_true("add_rcpp" %in% names(map))

    expect_true(grepl(map[["add_rcpp"]], "double add_rcpp(double x, double y)"))

    expect_true(grepl(map[["add_rcpp_default"]], "double add_rcpp_default(double x, double y = 1)"))
  })

  test_that("lookup_rcpp_local", {
    res <- lookup_rcpp_local("add_rcpp", "TestRcpp")
    expect_equal(length(res), 1L)

    expect_equal(res[[1]]$content, "double add_rcpp(double x, double y) {\n  return x + y;\n}")
  })

  test_that("lookup_rcpp", {
    res <- lookup_rcpp("add_rcpp", "TestRcpp")
    expect_equal(length(res), 1L)

    expect_equal(res[[1]]$content, "double add_rcpp(double x, double y) {\n  return x + y;\n}")
  })

})
