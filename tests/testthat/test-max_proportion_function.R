context("test-max_proportion_function.R")

# setup computational intensive data
data(toy)
k <- c(3, 4)
d <- dist(t(toy), method = "euclidean")
beta.clu.ward    <- hclust(d, method = "ward.D")
beta.clu.method2 <- hclust(d, method = "complete")

test_that("error for incorrect input", {
  expect_error(max_proportion_function(
    1, beta.clu.ward, beta.clu.method2, return.list = TRUE),
    'is_of_length'
  )
  expect_error(max_proportion_function(
    NA, beta.clu.ward, beta.clu.method2, return.list = TRUE),
    'is_of_length'
  )
  expect_error(max_proportion_function(
    c(1, 2, 3), beta.clu.ward, beta.clu.method2, return.list = TRUE),
    'is_of_length'
  )
  expect_error(max_proportion_function(
    c(1, 2), 1, beta.clu.method2, return.list = TRUE),
    'hclust'
  )
  expect_error(max_proportion_function(
    c(1, 2), beta.clu.ward, 1, return.list = TRUE),
    'hclust'
  )
  expect_error(max_proportion_function(
    c(1, 2), beta.clu.ward, beta.clu.method2, return.list = NA),
    'missing value'
  )
  expect_error(max_proportion_function(
    c(1, 2), beta.clu.ward, beta.clu.method2, return.list = c(TRUE,  TRUE)),
    'is_a_bool'
  )
})

test_that("correct output class", {
  expect_type(
    max_proportion_function(k, beta.clu.ward, beta.clu.method2),
    'integer'
  )
  expect_type(
    max_proportion_function(k, beta.clu.ward, beta.clu.method2,
      return.list = TRUE
    ),
    'list'
  )
})

test_that("correct known result", {
  expected <- 6L
  actual   <- max_proportion_function(k, beta.clu.ward, beta.clu.method2)

  actual_list <- max_proportion_function(k, beta.clu.ward, beta.clu.method2,
    return.list = TRUE
  )
  expect_equal(actual, expected)
  expect_equal_to_reference(actual_list, 'max_proportion_function-ref.RDS')
})
