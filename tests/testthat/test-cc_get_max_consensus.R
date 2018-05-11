context("test-cc_get_max_consensus.R")

# setup computational intensive data
data(toy)
k <- c(3, 4)
d <- dist(t(toy), method = "euclidean")
cluster_ward    <- hclust(d, method = "ward.D")
cluster_other <- hclust(d, method = "complete")

test_that("error for incorrect input", {
  expect_error(cc_get_max_consensus(
    1, cluster_ward, cluster_other, return_list = TRUE),
    'is_of_length'
  )
  expect_error(cc_get_max_consensus(
    NA, cluster_ward, cluster_other, return_list = TRUE),
    'is_of_length'
  )
  expect_error(cc_get_max_consensus(
    c(1, 2, 3), cluster_ward, cluster_other, return_list = TRUE),
    'is_of_length'
  )
  expect_error(cc_get_max_consensus(
    c(1, 2), 1, cluster_other, return_list = TRUE),
    'hclust'
  )
  expect_error(cc_get_max_consensus(
    c(1, 2), cluster_ward, 1, return_list = TRUE),
    'hclust'
  )
  expect_error(cc_get_max_consensus(
    c(1, 2), cluster_ward, cluster_other, return_list = NA),
    'missing value'
  )
  expect_error(cc_get_max_consensus(
    c(1, 2), cluster_ward, cluster_other, return_list = c(TRUE,  TRUE)),
    'is_a_bool'
  )
})

test_that("correct output class", {
  expect_type(
    cc_get_max_consensus(k, cluster_ward, cluster_other),
    'integer'
  )
  expect_type(
    cc_get_max_consensus(k, cluster_ward, cluster_other,
      return_list = TRUE
    ),
    'list'
  )
})

test_that("correct known result", {
  expected <- 6L
  actual   <- cc_get_max_consensus(k, cluster_ward, cluster_other)

  actual_list <- cc_get_max_consensus(k, cluster_ward, cluster_other,
    return_list = TRUE
  )
  expect_equal(actual, expected)
  expect_equal_to_reference(actual_list, 'cc_get_max_consensus-ref.RDS')
})
