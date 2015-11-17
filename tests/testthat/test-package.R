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
  res <- as_lookup(factorial, nme = "factorial")
  is_factorial(res)

  # already a lookup
  res <- as_lookup(res)

  # name of x (lookup called print.function)
  res <- as_lookup(factorial, nme = "x")

  # primitive function
  res <- as_lookup(max, nme = "x")
  expect_identical(res$name, "max")
  expect_identical(res$package, "base")
  expect_identical(res$type, "builtin")
  expect_identical(res$visible, TRUE)
  expect_identical(res$def, base::max)
})
test_that("works", {


p1 <- lookup(mean)
expect_equal(p1$type, c("S3 generic", "closure"))

p1 <- lookup(mean, all = TRUE)
expect_true(length(p1$S3_methods) > 1)

p2 <- lookup(base::mean)
expect_equal(p2$type, c("S3 generic", "closure"))

p2 <- lookup(base::mean, all = TRUE)
expect_true(length(p2$S3_methods) > 1)
})
