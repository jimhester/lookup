context("github-call")
with_github_package("jimhester/lookup/tests/testthat/TestRcpp", {
  s <- NULL
  test_that("fetch_symbol_map", {
    s <<- as.source_type("TestRcpp", "call", "add_call_impl")
    expect_equal(class(s), c("call_github", "call", "github"))

    s <<- fetch_symbol_map(s)
    expect_true(length(s$map_lines) > 1)

    expect_error(as.source_type("missing", "call"), "no package 'missing' was found")
  })

  test_that("parse_symbol_map", {
    s <<- parse_symbol_map(s)
    expect_true(length(s$map) > 1)
  })

  test_that("source_files", {
    s <<- source_files(s)
    expect_equal(basename(s$src_files), "add.c")
  })

  test_that("fetch_source", {
    s <<- fetch_source(s, s$src_files[[1]])
    expect_true(length(s$src_lines) > 1)
  })

  test_that("parse_source", {
    s <<- parse_source(s)
    expect_true(length(s$fun_lines) > 1)

    expect_true(s$fun_start > 1)
    expect_true(s$fun_end > s$fun_start)
  })

  test_that("lookup_function", {
    res <- lookup_function("add_call_impl", "call", "TestRcpp")

    expect_true(nchar(res$content) > 0)
    expect_equal(res$remote_type, "github")
    expect_equal(res$type, "call")
    expect_equal(res$language, "c")

    expect_null(lookup_function("missing", "call", "TestRcpp"))
  })
})
