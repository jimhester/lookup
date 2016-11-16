context("call")

test_that("has_call works", {
  # non_function arguments just return FALSE
  expect_false(has_call("blah"))

  # types that don't exist in the function return FALSE
  expect_false(has_call(identical, "wrong"))

  expect_true(has_call(identical, ".Internal"))
})

test_that("call_name works", {
  expect_equal(call_name(identical, "wrong"), character(0))

  expect_equal(call_name(identical, ".Internal"), "identical")

  expect_equal(call_name(print.default, ".Internal"), "print.default")
})
