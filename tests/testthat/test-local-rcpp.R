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

  context("local-rcpp")
  test_that("rcpp_symbol_map", {

    map <- rcpp_symbol_map_local("TestRcpp")

    expect_true("add_rcpp" %in% names(map))

    expect_true(grepl(map[["add_rcpp"]], "double add_rcpp(double x, double y)"))

    expect_true(grepl(map[["add_rcpp_default"]], "double add_rcpp_default(double x, double y = 1)"))
  })

  test_that("lookup_rcpp_local", {
    res <- lookup_rcpp_local("add_rcpp", "TestRcpp")
    expect_equal(length(x), 1L)

    expect_equal(x[[1]]$content, "double add_rcpp(double x, double y) {\n  return x + y;\n}")
  })

  context("local-c")
  test_that("c_symbol_map", {
    map <- c_symbol_map_local("TestRcpp")
    expect_true("add_c_impl" %in% names(map))

    expect_equal("add_c_impl", map[["add_c_impl"]])
  })
})
