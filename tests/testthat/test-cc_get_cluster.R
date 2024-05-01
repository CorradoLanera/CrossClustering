context("test-cc_get_cluster.R")

test_that("error for incorrect input", {
  ok_list  <- list(1, 2)
  ko_list  <- c(1, 2)
  ok_n_low <- 1
  ko_n     <- "a"
  expect_error(
    cc_get_cluster(ko_list, ok_n_low),
    "Must be of class 'list'"
  )
  expect_error(
    cc_get_cluster(ok_list, ko_n),
    "Must be of class 'integerish'"
  )
  expect_error(
    cc_get_cluster(ok_list, ok_n_low),
    "at least the number"
  )
  expect_error(
    cc_get_cluster(ok_list, 1.4),
    "Must be of class 'integerish'"
  )
})

test_that("output class is integer", {
  cluster_list <- list(1:2, 3:4, 5:6)
  n <- 7
  expect_type(cc_get_cluster(cluster_list, n), "integer")
})

test_that("correct known result", {
  cluster_list <- list(1:2, 3:4, 5:6)
  n <- 7
  expected <- c(2L, 2L, 3L, 3L, 4L, 4L, 1L)
  actual   <- cc_get_cluster(cluster_list, n)
  expect_equal(actual, expected)
})
