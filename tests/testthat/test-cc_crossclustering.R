context("test-cc_crossclustering.R")

# Provide high computational data
data(toy)
dist <- dist(t(toy), method = "euclidean")
k_w_min <- 2
k_w_max <- 5
k2_max  <- 6
out     <- TRUE


expected <-  structure(list(
  Optimal_cluster = 3L,
  Cluster_list    = list(1:2, 3:4, 5:6),
  Silhouette      = 0.840520401573931,
  n_total         = 7L,
  n_clustered     = 6L
),
.Names = c("Optimal_cluster", "Cluster_list", "Silhouette", "n_total",
           "n_clustered"
))




test_that("error for incorrect input", {
  expect_error(
    cc_crossclustering(1, k_w_min, k_w_max, k2_max, out, 'complete'),
    'is not in any of the classes'
  )
  expect_error(
    cc_crossclustering(dist, -1, k_w_max, k2_max, out, 'complete'),
    'is_positive'
  )
  expect_error(
    cc_crossclustering(dist, 1.2, k_w_max, k2_max, out, 'complete'),
    'is_equal_to'
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, -1, k2_max, out, 'complete'),
    'is_less_than'
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, 3.2, k2_max, out, 'complete'),
    'is_equal_to'
  )
  expect_error(
    cc_crossclustering(dist, 3, 2, k2_max, out, 'complete'),
    'is_less_than'
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, k_w_max, -1, out, 'complete'),
    'is_less_than'
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, k_w_max, 3.2, out, 'complete'),
    'is_equal_to'
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, 2, out, 'complete'),
    'is_less_than'
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, k2_max, 5, 'complete'),
    'is_a_bool'
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, k2_max, out, 1),
    'is_character'
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, k2_max, out, 'a'),
    'should be one of '
  )
})

test_that("correct output class", {
  current <- cc_crossclustering(dist,
    k_w_min, k_w_max, k2_max, out, 'complete'
  )

  expect_type(current, 'list')
  expect_equal(names(current),
    c(
      "Optimal_cluster", "Cluster_list", "Silhouette", "n_total",
      "n_clustered"
    )
  )

  expect_type(current[['Optimal_cluster']], 'integer')
  expect_type(current[['Cluster_list']], 'list')
  expect_type(current[['Silhouette']], 'double')
  expect_type(current[['n_total']], 'integer')
  expect_type(current[['n_clustered']], 'integer')
  expect_equal(current, expected)
})
