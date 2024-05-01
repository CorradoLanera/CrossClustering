context("test-cc_test_ari.R")

test_that("error for incorrect input", {
  expect_error(cc_test_ari("a", 1), "Must be of class 'numeric'")
  expect_error(cc_test_ari(1, "a"), "Must be of class 'numeric'")
  expect_error(
    cc_test_ari(c(1, 2), 2),
    paste(
      "Assertion on",
      "'length\\(ground_truth\\) == length\\(partition\\)' failed:",
      "Must be TRUE\\."
    )
  )
  expect_error(cc_test_ari(1, NA), "Must be of class 'numeric'")
  expect_error(cc_test_ari(1, c(NA_real_)), "May not contain missing values")
  expect_error(cc_test_ari(1, NaN), "May not contain missing values")
  expect_error(cc_test_ari(c(NA_real_), 1), "May not contain missing values")
  expect_error(cc_test_ari(NaN, 1), "May not contain missing values")
})

test_that("output class is a list", {
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))
  expect_type(cc_test_ari(ground_truth, partition), "list")
})

test_that("output list has all the correct componet", {
  ground_truth  <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition     <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  expected <- c(
    "rand", "expected_rand", "adjusted_rand", "var_ari", "nari",
    "p_value"
  )
  actual   <- purrr::map_lgl(
    cc_test_ari(ground_truth, partition),
    is.numeric
  )
  expect_equal(names(actual), expected)
  expect_equivalent(actual, rep(TRUE, 6))
})


test_that("correct known result", {
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  actual <- cc_test_ari(ground_truth, partition)
  expect_equal_to_reference(actual, "cc_test_ari-ref.RDS")
})
