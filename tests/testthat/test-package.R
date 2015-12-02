test_that("as_lookup works", {
  is_factorial <- function(x) {
    expect_identical(x$name, "factorial")
    expect_identical(x$package, "base")
    expect_identical(x$type, "closure")
    expect_identical(x$visible, TRUE)
    expect_identical(x$def, base::factorial)
  }

  # function as text
  res <- as_lookup("factorial")
  is_factorial(res)

  # function as body and name as text
  res <- as_lookup(factorial, name = "factorial")
  is_factorial(res)

  # already a lookup
  res <- as_lookup(res)

  # name of x (lookup called print.function)
  res <- as_lookup(factorial, name = "x")

  # primitive function
  res <- as_lookup(max, name = "x")
  expect_identical(res$name, "max")
  expect_identical(res$package, "base")
  expect_identical(res$type, "builtin")
  expect_identical(res$visible, TRUE)
  expect_identical(res$def, base::max)
})

test_that("lookup for a simple function works", {
  p1 <- lookup(replace)
  expect_equal(p1$type, "closure")

  expect_identical(lookup(replace), lookup(base::replace))
})

test_that("lookup for simple S3 generic works", {

  # with is a R only S3 generic, with only 1 method in Recommended packages.
  p1 <- lookup(with)
  expect_equal(p1$type, c("S3 generic", "closure"))

  expect_true(length(p1$S3_methods) >= 1)

  p2 <- lookup(base::with)
  expect_equal(p2$type, c("S3 generic", "closure"))

  p2 <- lookup(base::with, all = TRUE)
  expect_true(length(p2$S3_methods) >= 1)

  expect_identical(p1, p2)
})

test_that("names_map works properly", {
  x <- readRDS("names.c.rds")
  m <- names_map(x)

  expect_identical(m[["identical"]], "do_identical")
  expect_identical(m[["makeLazy"]], "do_makelazy")
  expect_identical(m[["<="]], "do_relop")
  expect_identical(m[["-"]], "do_arith")
  expect_identical(m[["setEncoding"]], "do_setencoding")
})

test_that("auto_name works properly", {
  t1 <- c("one", "two")
  expect_identical(auto_name(t1), t1)
  t2 <- c("blah", "")
  expect_identical(auto_name(t2), c("blah", "2"))
  t3 <- c("", "blah")
  expect_identical(auto_name(t3), c("1", "blah"))
  t3 <- c("blah", "", "blah2")
  expect_identical(auto_name(t3), c("blah", "2", "blah2"))
})

test_that("assert works properly", {
  expect_error(assert(), "is missing, with no default")
  expect_error(assert(0), "Error : \n")
  expect_error(assert(FALSE), "Error : \n")
  expect_null(assert(TRUE))

  expect_error(assert(FALSE, "test"), "Error : test\n")
})

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}

test_that("captures works properly", {
  x <- c("one 1", "two 2", "three 3")
  re <- "([[:digit:]]+)"
  m1 <- regexpr(re, x)

  expect_error(captures(c(1, 2, 3), m1), "must be a character vector")
  expect_error(captures(x, m1), "must be the result")

  m2 <- regexpr(re, x, perl = TRUE)
  expect_identical(captures(x, m2),
    data_frame("1" = c("1", "2", "3")))

  re2 <- "([[:alpha:]]+) ([[:digit:]]+)"
  m2 <- regexpr(re2, x, perl = TRUE)
  expect_identical(captures(x, m2),
    data_frame("1" = c("one", "two", "three"),
       "2" = c("1", "2", "3")))

  re3 <- "(?<char>[[:alpha:]]+) (?<num>[[:digit:]]+)"
  m2 <- regexpr(re2, x, perl = TRUE)
  expect_identical(captures(x, m2),
    data_frame("char" = c("one", "two", "three"),
       "2" = c("num", "2", "3")))
})

     #require(stats)
     
     #methods(summary)
     #methods(class = "aov")    # S3 class
     ### The same, with more details and more difficult to read:
     #print(methods(class = "aov"), byclass=FALSE)
     #methods("[[")             # uses C-internal dispatching
     #methods("$")
     #methods("$<-")            # replacement function
     #methods("+")              # binary operator
     #methods("Math")           # group generic
     #require(graphics)
     #methods("axis")           # looks like a generic, but is not
     
     #if(require(Matrix)) {
     #print(methods(class = "Matrix"))  # S4 class
     #m <- methods("dim")       # S3 and S4 methods
     #print(m)
     #print(attr(m, "info"))    # more extensive information
