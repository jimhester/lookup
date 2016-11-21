with_local_package("TestRcpp", {
  context("local-rcpp")
  test_that("fetch_symbol_map", {
    s <- as.source_type("TestRcpp", "rcpp")
    s <- fetch_symbol_map(s)
    expect_true(length(s$map_lines) > 1)

    expect_error(as.source_type("missing", "rcpp"), "no package 'missing' was found")
  })

  test_that("parse_symbol_map", {
    s <- as.source_type("TestRcpp", "rcpp")
    s <- fetch_symbol_map(s)

    s <- parse_symbol_map(s)
    expect_true(length(s$map) > 1)
  })

  test_that("source_files", {
    s <- as.source_type("TestRcpp", "rcpp")
    s <- fetch_symbol_map(s)
    s <- parse_symbol_map(s)
    s <- source_files(s, "add_rcpp")
    expect_equal(basename(s$src_files), "package.cpp")
  })
})
