context("internal")

  s <- NULL
  test_that("fetch_symbol_map", {
    s <<- internal_source("grep")
    expect_equal(class(s), "internal")

    s <<- fetch_symbol_map(s)
    expect_true(length(s$map_lines) > 1)
  })

  test_that("parse_symbol_map", {
    s <<- parse_symbol_map(s)
    expect_true(length(s$map) > 0)

    m <- s$map
    expect_identical(m[["identical"]], "do_identical")
    expect_identical(m[["makeLazy"]], "do_makelazy")
    expect_identical(m[["<="]], "do_relop")
    expect_identical(m[["-"]], "do_arith")
    expect_identical(m[["setEncoding"]], "do_setencoding")
  })

  test_that("source_files", {
    s <<- source_files(s)
    expect_equal(s$src_files, "src/main/grep.c")
  })

  test_that("fetch_source", {
    s <<- fetch_source(s, s$src_files[[1]])
    expect_true(length(s$src_lines) > 1)
  })

  test_that("parse_source", {
    s <<- parse_source(s, s$map[s$search])
    expect_true(length(s$fun_lines) > 1)

    expect_true(s$fun_start > 1)
    expect_true(s$fun_end > s$fun_start)
  })

  test_that("lookup_function", {
    res <- lookup_function("grep", "internal")

    expect_true(nchar(res$content) > 0)
    expect_equal(res$remote_type, "internal")
    expect_equal(res$type, "internal")
    expect_equal(res$language, "c")
  })
