context("test-consensus_cluster.R")

# setup computational intensive data
data(toy)
k <- c(3, 4)
d <- dist(t(toy), method = "euclidean")
cluster_ward    <- hclust(d, method = "ward.D")
cluster_other <- hclust(d, method = "complete")

test_that("error for incorrect input", {
  expect_error(
    consensus_cluster(1, cluster_ward, cluster_other),
    "Must be of length == 2"
  )
  expect_error(
    consensus_cluster(NA, cluster_ward, cluster_other),
    "Must be of class 'numeric'"
  )
  expect_error(
    consensus_cluster(c(1, 2, 3), cluster_ward, cluster_other),
    "Must be of length == 2"
  )
  expect_error(
    consensus_cluster(c(1, 2), 1, cluster_other),
    "hclust"
  )
  expect_error(
    consensus_cluster(c(1, 2), cluster_ward, 1),
    "hclust"
  )
})

test_that("correct output class", {
  expect_type(
    consensus_cluster(k, cluster_ward, cluster_other),
    "list"
  )
})

test_that("correct known result", {
  expected <- 6L
  actual   <- consensus_cluster(k,
    cluster_ward,
    cluster_other
  )[["max_consensus"]]

  actual_list <- consensus_cluster(k, cluster_ward, cluster_other)
  expect_equal(actual, expected)
  expect_equal_to_reference(actual_list, "consensus_cluster-ref.RDS")
})
