context("external")

if (FALSE) { # TODO: find another package that installs properly on travis
with_cran_package("lassoshooting", {
  s <- NULL
  test_that("fetch_symbol_map", {
    s <<- as.source_type("lassoshooting", "external", "ccd")
    expect_equal(class(s), c("external_cran", "external", "cran"))

    s <<- fetch_symbol_map(s)
    expect_true(length(s$map_lines) > 1)

    expect_error(as.source_type("missing", "external"), "no package 'missing' was found")
  })

  test_that("parse_symbol_map", {
    s <<- parse_symbol_map(s)
    expect_true(length(s$map) > 0)
  })

  test_that("source_files", {
    s <<- source_files(s)
    expect_equal(basename(s$src_files), c("ccd.h", "ccd_common.c", "ccd_r.c"))
  })

  test_that("fetch_source", {
    s <<- fetch_source(s, s$src_files[[3]])
    expect_true(length(s$src_lines) > 1)
  })

  test_that("parse_source", {
    s <<- parse_source(s)
    expect_true(length(s$fun_lines) > 1)

    expect_true(s$fun_start > 1)
    expect_true(s$fun_end > s$fun_start)
  })

  test_that("lookup_function", {
    res <- lookup_function("ccd", "external", "lassoshooting")

    expect_true(nchar(res$content) > 0)
    expect_equal(res$remote_type, "cran")
    expect_equal(res$type, "external")
    expect_equal(res$language, "c")

    expect_null(lookup_function("missing", "external", "lassoshooting"))
  })
})
}

test_that("lookup_function base functions", {
  res <- lookup_function("C_doD", "external", "stats")

  expect_true(nchar(res$content) > 0)
  expect_equal(res$remote_type, "base")
  expect_equal(res$type, "external")
  expect_equal(res$language, "c")

  expect_null(lookup_function("missing", "external", "stats"))
})
