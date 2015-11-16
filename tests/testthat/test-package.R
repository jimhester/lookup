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
