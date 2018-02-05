context("test-CrossClustering.R")

# Provide high computational data
data(toy)
d <- dist(t(toy), method = "euclidean")
k.w.min <- 2
k.w.max <- 5
k2.max  <- 6
out     <- TRUE


expected <-  structure(list(
  Optimal.cluster = 3L,
  Cluster.list    = list(1:2, 3:4, 5:6),
  Silhouette      = 0.840520401573931,
  n.total         = 7L,
  n.clustered     = 6L
),
.Names = c("Optimal.cluster", "Cluster.list", "Silhouette", "n.total",
           "n.clustered"
))




test_that("error for incorrect input", {
  expect_error(
    CrossClustering(1, k.w.min, k.w.max, k2.max, out, 'complete'),
    'is not in any of the classes'
  )
  expect_error(CrossClustering(d, -1, k.w.max, k2.max, out, 'complete'),
               'is_positive'
  )
  expect_error(CrossClustering(d, 1.2, k.w.max, k2.max, out, 'complete'),
               'is_equal_to'
  )
  expect_error(CrossClustering(d, k.w.min, -1, k2.max, out, 'complete'),
               'is_less_than'
  )
  expect_error(CrossClustering(d, k.w.min, 3.2, k2.max, out, 'complete'),
               'is_equal_to'
  )
  expect_error(CrossClustering(d, 3, 2, k2.max, out, 'complete'),
               'is_less_than'
  )
  expect_error(CrossClustering(d, k.w.min, k.w.max, -1, out, 'complete'),
               'is_less_than'
  )
  expect_error(CrossClustering(d, k.w.min, k.w.max, 3.2, out, 'complete'),
               'is_equal_to'
  )
  expect_error(CrossClustering(d, 3, k.w.max, 2, out, 'complete'),
               'is_less_than'
  )
  expect_error(CrossClustering(d, 3, k.w.max, k2.max, 5, 'complete'),
               'is_a_bool'
  )
  expect_error(CrossClustering(d, 3, k.w.max, k2.max, out, 1),
               'is_character'
  )
  expect_error(CrossClustering(d, 3, k.w.max, k2.max, out, 'a'),
               'should be one of '
  )
})

test_that("correct output class", {
  current <- CrossClustering(d, k.w.min, k.w.max, k2.max, out, 'complete')

  expect_type(current, 'list')
  expect_equal(names(current),
    c(
      "Optimal.cluster", "Cluster.list", "Silhouette", "n.total",
      "n.clustered"
    )
  )

  expect_type(current[['Optimal.cluster']], 'integer')
  expect_type(current[['Cluster.list']], 'list')
  expect_type(current[['Silhouette']], 'double')
  expect_type(current[['n.total']], 'integer')
  expect_type(current[['n.clustered']], 'integer')
  expect_equal(current, expected)
})
