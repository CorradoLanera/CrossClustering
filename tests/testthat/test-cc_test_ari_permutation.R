context("test-cc_test_ari_permutation.R")

test_that("error for incorrect input", {
  expect_error(
    cc_test_ari_permutation("a", 1),
    "Must be of class 'numeric'"
  )
  expect_error(
    cc_test_ari_permutation(1, "a"),
    "Must be of class 'numeric'"
  )
  expect_error(
    cc_test_ari_permutation(c(1, 2), 2),
    paste(
      "Assertion on",
      "'length\\(ground_truth\\) == length\\(partition\\)' failed:",
      "Must be TRUE\\."
    )
  )
  expect_error(
    cc_test_ari_permutation(1, NA),
    "Must be of class 'numeric'"
  )
  expect_error(
    cc_test_ari_permutation(1, c(NA_real_)),
    "May not contain missing values"
  )
  expect_error(
    cc_test_ari_permutation(1, NaN),
    "May not contain missing values"
  )
  expect_error(
    cc_test_ari_permutation(c(NA_real_), 1),
    "May not contain missing values"
  )
  expect_error(
    cc_test_ari_permutation(NaN, 1),
    "May not contain missing values"
  )
})

test_that("output class is a list", {
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))
  expect_s3_class(
    cc_test_ari_permutation(ground_truth, partition),
    "data.frame"
  )
})

test_that("output list has all the correct componet", {
  ground_truth  <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition     <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  expected <- c("ari", "p_value")
  ps_ari <- cc_test_ari_permutation(ground_truth, partition)
  actual   <- purrr::map_lgl(ps_ari, is.numeric)
  expect_equal(names(actual), expected)
  expect_equivalent(actual, rep(TRUE, 2))
  checkmate::expect_numeric(ps_ari[["p_value"]], lower = 0, upper = 1)
})


test_that("correct known result", {
  set.seed(1234)
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  # for the moment we whant "p-value"
  expected <- data.frame(ari = 1, "p_value" = 0.001, check.names = FALSE)
  actual   <- cc_test_ari_permutation(ground_truth, partition)
  expect_equal(actual, expected)
})
