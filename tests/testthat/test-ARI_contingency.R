context("test-ARI_contingency.R")

test_that("error for incorrect input", {
  mat <- matrix(1)
  expect_error(ARI_contingency('a', 1), 'is_matrix')
  expect_error(ARI_contingency(matrix('a'), 1), 'is_numeric')
  expect_error(ARI_contingency(matrix(-1), 1), 'is_positive')
  expect_error(ARI_contingency(matrix(1.4), 1), 'is_equal_to')
  expect_error(ARI_contingency(mat, 'a'), 'is_a_double')
  expect_error(ARI_contingency(mat, 1), 'is_proportion')
  expect_error(ARI_contingency(mat, 0), 'is_proportion')
  expect_error(ARI_contingency(mat, 2), 'is_proportion')
  expect_error(ARI_contingency(mat, -1), 'is_proportion')
  expect_error(ARI_contingency(mat, 0.5, 'a'), 'is_a_number')
  expect_error(ARI_contingency(mat, 0.5, -1), 'is_greater_than_or_equal_to')
})

test_that("correct output class", {
  mat   <- matrix(c(4,5))
  alpha <- 0.05
  expect_type(
    ARI_contingency(mat = mat, alpha = alpha),
    'double'
  )
  expect_length(
    ARI_contingency(mat = mat, alpha = alpha),
    3
  )
})

test_that("correct known result", {
  mat <- matrix(c(4, 5, 3, 3, 8, 4), ncol = 3, byrow = TRUE)

  reference <- structure(
    c(-0.03, -0.13, 0.07),
    .Names = c("ari", "ci_low", "ci_high")
  )
  actual    <- ARI_contingency(mat = mat)
  expect_equal(actual, reference)
})
