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
test_that("lookup for simple S3 generic works", {

# with is a R only S3 generic, with only 1 method using Recommended packages.

p1 <- lookup(with)
expect_equal(p1$type, c("S3 generic", "closure"))

p1 <- lookup(with, all = TRUE)
expect_true(length(p1$S3_methods) >= 1)

p2 <- lookup(base::with)
expect_equal(p2$type, c("S3 generic", "closure"))

p2 <- lookup(base::with, all = TRUE)
expect_true(length(p2$S3_methods) >= 1)
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
