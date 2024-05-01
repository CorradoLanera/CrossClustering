context("test-utils.R")

test_that("prune_zero_tail prune matrix and only if correctly provided", {
  mat_ok <- diag(c(1, 2, 3, 0, 0, 0, 0))
  mat_ko <- diag(c(1, 2, 3, 0, 0, 4, 0))

  expected <- diag(c(1, 2, 3))
  expect_equal(prune_zero_tail(mat_ok), expected)
  expect_equal(prune_zero_tail(expected), expected)
  expect_error(prune_zero_tail(mat_ko), "cannot have non-zeros after")
})


test_that("is_zero", {
  expect_true(is_zero(0))
  expect_false(is_zero(1))
  expect_error(is_zero("a"), "Must be of class 'numeric'")
})
