context("test-cc_crossclustering.R")

# Provide high computational data
data(toy)
dist <- dist(t(toy), method = "euclidean")
k_w_min <- 2
k_w_max <- 5
k2_max  <- 6
out     <- TRUE


expected <-
  structure(list(cluster_1 = 1:2, cluster_2 = 3:4, cluster_3 = 5:6),
    optimal_cluster = 3L,
    silhouette = 0.840520401573931,
    n_total = 7L,
    n_clustered = 6L,
    input = list(
      dist = structure(
        c(
          0.278767553844071, 31.4684914051983, 31.7017423869459,
          15.8457009710004, 15.7691512786285, 30.4151785838636,
          31.4269010762807, 31.6601486681976, 15.8880937927878,
          15.8118612631759, 30.4564427729642, 0.444267526306529,
          47.3045592447933, 47.2321271743607, 61.8775059744892,
          47.5400523222032, 47.4667735359252, 62.1119298535437,
          0.387650341413089, 14.6042509896801, 14.6614841506956
        ),
        Size   = 7L,
        Labels = c(
          "Sample1", "Sample2", "Sample3", "Sample4", "Sample5",
          "Sample6", "Sample7"
        ),
        Diag = FALSE,
        Upper = FALSE,
        method = "euclidean",
        call = quote(dist(x = t(toy), method = "euclidean")),
        class = "dist"
      ),
      k_w_min = 2,
      k_w_max = 5,
      k2_max = 6,
      out = TRUE,
      method = "complete"
    ),
    class = "crossclustering"
  )




test_that("error for incorrect input", {
  expect_error(
    cc_crossclustering(1, k_w_min, k_w_max, k2_max, out, "complete"),
    "Must inherit from class 'dist'"
  )
  expect_error(
    cc_crossclustering(dist, -1, k_w_max, k2_max, out, "complete"),
    "All elements must be > 0"
  )
  expect_error(
    cc_crossclustering(dist, 1.2, k_w_max, k2_max, out, "complete"),
    "Must be of class 'integerish'"
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, -1, k2_max, out, "complete"),
    "Element 1 is not >= 2"
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, 3.2, k2_max, out, "complete"),
    "Must be of type 'integerish'"
  )
  expect_error(
    cc_crossclustering(dist, 3, 2, k2_max, out, "complete"),
    "Element 1 is not >= 3"
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, k_w_max, -1, out, "complete"),
    "All elements must be > 0"
  )
  expect_error(
    cc_crossclustering(dist, k_w_min, k_w_max, 3.2, out, "complete"),
    "Must be of class 'integerish'"
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, 2, out, "complete"),
    "Element 1 is not >= 3"
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, k2_max, 5, "complete"),
    "Must be of class 'logical'"
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, k2_max, out, 1),
    "must be NULL or a character vector"
  )
  expect_error(
    cc_crossclustering(dist, 3, k_w_max, k2_max, out, "a"),
    "should be one of"
  )
})

test_that("correct output class", {
  current <- cc_crossclustering(dist,
    k_w_min, k_w_max, k2_max, out, "complete"
  )

  expect_type(current, "list")
  expect_is(current, "crossclustering")
  expect_equal(names(current),
    paste("cluster", seq_along(current), sep = "_")
  )

  expect_type(attr(current, "optimal_cluster"), "integer")
  expect_type(attr(current, "silhouette"), "double")
  expect_type(attr(current, "n_total"), "integer")
  expect_type(attr(current, "n_clustered"), "integer")
  expect_equal(current, expected)
})
