context("test-ari.R")

test_that("error (only) for incorrect input", {
  mat      <- matrix(1)
  mat_zero <- matrix(c(1, 0, 1, 1), 2, 2)

  expect_error(ari("a", 1), "Must be of type 'matrix'")
  expect_error(ari(matrix("a"), 1), "Must be of class 'integerish'")
  expect_error(ari(matrix(-1), 1), "All elements must be >= 0")
  expect_error(ari(matrix(1.4), 1), "Must be of class 'integerish'")
  expect_error(ari(mat, "a"), "Must be of class 'double'")
  expect_error(ari(mat, 1), "All elements must be < 1")
  expect_error(ari(mat, 0), "All elements must be > 0")
  expect_error(ari(mat, 2), "All elements must be < 1")
  expect_error(ari(mat, -1), "All elements must be > 0")
  expect_error(ari(mat, 0.5, "a"), "Must be of class 'integerish'")
  expect_error(ari(mat, 0.5, -1), "All elements must be >= 0")

  expect_type(suppressWarnings(ari(mat_zero)), "list")
})

test_that("correct output class", {
  mat   <- matrix(c(4, 5))
  alpha <- 0.05
  expect_type(
    suppressWarnings(ari(mat = mat, alpha = alpha)),
    "list"
  )
  expect_length(
    suppressWarnings(ari(mat = mat, alpha = alpha)),
    4
  )
  expect_is(suppressWarnings(ari(mat = mat, alpha = alpha)),
    "ari"
  )
})

test_that("correct known result", {
  set.seed(1234)
  mat <- matrix(c(4, 5, 3, 3, 8, 4), ncol = 3, byrow = TRUE)

  reference <-
    structure(list(
      ari = structure(-0.03, ci = c(lower = -0.13, upper = 0.07)),
      input = list(
        mat = structure(c(4, 3, 5, 8, 3, 4), .Dim = 2:3),
        alpha = 0.05
      ),
      partitions = list(
        c(
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
        ),
        c(
          1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
          2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L
        )
      ),
      p_values = c(
        test_ari = 4.231622463546e-07,
        test_ari_permutation = 7.000000e-03
      )
    ),
    class = "ari")

  actual    <- suppressWarnings(ari(mat = mat))

  expect_equal(actual, reference)
})

test_that("Steinley known result", {
  set.seed(1234)
  mat <- matrix(c(2, 1, 0, 0, 2, 1), ncol = 3, byrow = TRUE)

  reference <- structure(
    list(
      ari = structure(0.1176,
        ci = c(lower = -0.4107, upper = 0.646)
      ),
      input = list(
        mat = structure(c(2, 0, 1, 2, 0, 1), .Dim = 2:3),
        alpha = 0.05
      ),
      partitions = list(
        c(1L, 1L, 1L, 2L, 2L, 2L),
        c(1L, 1L, 2L, 2L, 2L, 3L)
      ),
      p_values = c(test_ari = 0.331260291770029, test_ari_permutation = 1)
    ),
    class = "ari"
  )

  actual <- suppressWarnings(ari(mat = mat, digits = 4))
  expect_equal(actual, reference)
})
