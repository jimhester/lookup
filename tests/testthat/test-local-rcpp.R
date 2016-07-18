context("local-rcpp")

test_that("rcpp_symbol_map", {
  map <- rcpp_symbol_map_local("TestRcpp")

  expect_true("add" %in% names(map))

  expect_true(grepl(map[["add"]], "double add(double x, double y)"))

  expect_true(grepl(map[["add_default"]], "double add_default(double x, double y = 1)"))
})
