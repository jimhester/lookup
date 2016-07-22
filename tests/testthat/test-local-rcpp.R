context("local-rcpp")
local({
  if (!suppressWarnings(require("TestRcpp", quietly = TRUE))) {
    lib <- tempfile()
    on.exit(unlink(lib, recursive = TRUE), add = TRUE)
    dir.create(lib)
    libpath <- .libPaths()
    on.exit(.libPaths(libpath), add = TRUE)
    .libPaths(lib)
    install.packages("TestRcpp", repo = NULL, type = "source", quiet = TRUE)
  }

  test_that("rcpp_symbol_map", {

    map <- rcpp_symbol_map_local("TestRcpp")

    expect_true("add_rcpp" %in% names(map))

    expect_true(grepl(map[["add_rcpp"]], "double add_rcpp(double x, double y)"))

    expect_true(grepl(map[["add_rcpp_default"]], "double add_rcpp_default(double x, double y = 1)"))
  })
})
