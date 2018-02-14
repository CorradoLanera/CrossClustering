context("test-cc_max_proportion.R")

# setup computational intensive data
data(toy)
k <- c(3, 4)
d <- dist(t(toy), method = "euclidean")
beta_clu_ward    <- hclust(d, method = "ward.D")
beta_clu_method2 <- hclust(d, method = "complete")

test_that("error for incorrect input", {
  expect_error(cc_max_proportion(
    1, beta_clu_ward, beta_clu_method2, return_list = TRUE),
    'is_of_length'
  )
  expect_error(cc_max_proportion(
    NA, beta_clu_ward, beta_clu_method2, return_list = TRUE),
    'is_of_length'
  )
  expect_error(cc_max_proportion(
    c(1, 2, 3), beta_clu_ward, beta_clu_method2, return_list = TRUE),
    'is_of_length'
  )
  expect_error(cc_max_proportion(
    c(1, 2), 1, beta_clu_method2, return_list = TRUE),
    'hclust'
  )
  expect_error(cc_max_proportion(
    c(1, 2), beta_clu_ward, 1, return_list = TRUE),
    'hclust'
  )
  expect_error(cc_max_proportion(
    c(1, 2), beta_clu_ward, beta_clu_method2, return_list = NA),
    'missing value'
  )
  expect_error(cc_max_proportion(
    c(1, 2), beta_clu_ward, beta_clu_method2, return_list = c(TRUE,  TRUE)),
    'is_a_bool'
  )
})

test_that("correct output class", {
  expect_type(
    cc_max_proportion(k, beta_clu_ward, beta_clu_method2),
    'integer'
  )
  expect_type(
    cc_max_proportion(k, beta_clu_ward, beta_clu_method2,
      return_list = TRUE
    ),
    'list'
  )
})

test_that("correct known result", {
  expected <- 6L
  actual   <- cc_max_proportion(k, beta_clu_ward, beta_clu_method2)

  actual_list <- cc_max_proportion(k, beta_clu_ward, beta_clu_method2,
    return_list = TRUE
  )
  expect_equal(actual, expected)
  expect_equal_to_reference(actual_list, 'cc_max_proportion-ref.RDS')
})
