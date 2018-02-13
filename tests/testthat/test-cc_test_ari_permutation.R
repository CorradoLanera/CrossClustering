context("test-cc_test_ari_permutation.R")

test_that("error for incorrect input", {
  expect_error(cc_test_ari_permutation('a', 1), 'is_numeric')
  expect_error(cc_test_ari_permutation(1, 'a'), 'is_numeric')
  expect_error(cc_test_ari_permutation(c(1, 2), 2), 'are_same_length')
  expect_error(cc_test_ari_permutation(1, NA), 'is_numeric')
  expect_error(cc_test_ari_permutation(1, c(NA_real_)), 'is_not_na')
  expect_error(cc_test_ari_permutation(1, NaN), 'is_not_na')
  expect_error(cc_test_ari_permutation(c(NA_real_), 1), 'is_not_na')
  expect_error(cc_test_ari_permutation(NaN, 1), 'is_not_na')
})

test_that("output class is a list", {
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))
  expect_s3_class(
    cc_test_ari_permutation(ground_truth, partition),
    'data.frame'
  )
})

test_that("output list has all the correct componet", {
  ground_truth  <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition     <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  expected <- c('Stat', 'p-value')
  ps_ari <- cc_test_ari_permutation(ground_truth, partition)
  actual   <- purrr::map_lgl(ps_ari, is.numeric)
  expect_equal(names(actual), expected)
  expect_equivalent(actual, rep(TRUE, 2))
  expect_true(assertive::is_proportion(ps_ari[['p-value']]))
})


test_that("correct known result", {
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  # for the moment we whant "p-value"
  expected <- data.frame(Stat = 1, 'p-value' = 0.001, check.names = FALSE)
  actual <- cc_test_ari_permutation(ground_truth, partition)
  expect_equal(actual, expected)
})
