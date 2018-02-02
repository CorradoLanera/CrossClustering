context("test-SignificanceARI.R")

test_that("output class is a list", {
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))
  expect_type(SignificanceARI(ground_truth, partition), 'list')
})

test_that("output list has all the correct componet", {
  ground_truth  <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition     <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  expected <- c(
    'Rand', 'ExpectedRand', 'AdjustedRand', 'varARI', 'NARI', 'p.value'
  )
  actual   <- purrr::map_lgl(
    SignificanceARI(ground_truth, partition),
    is.numeric
  )
  expect_equal(names(actual), expected)
  expect_equivalent(actual, rep(TRUE, 6))
})


test_that("correct known result", {
  ground_truth <- c(rep(1L, 22), rep(2L, 24), rep(3L, 5))
  partition    <- c(rep(3L, 22), rep(2L, 24), rep(1L, 5))

  actual <- SignificanceARI(ground_truth, partition)
  expect_equal_to_reference(actual, 'SignificanceARI-ref.RDS')
})

test_that("error for incorrect input", {
  expect_error(SignificanceARI('a', 1), 'is_numeric')
  expect_error(SignificanceARI(1, 'a'), 'is_numeric')
  expect_error(SignificanceARI(c(1, 2), 2), 'are_same_length')
  expect_error(SignificanceARI(1, NA), 'is_numeric')
  expect_error(SignificanceARI(1, c(NA_real_)), 'is_not_na')
  expect_error(SignificanceARI(1, NaN), 'is_not_na')
  expect_error(SignificanceARI(c(NA_real_), 1), 'is_not_na')
  expect_error(SignificanceARI(NaN, 1), 'is_not_na')
})
