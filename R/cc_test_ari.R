#' A test for testing the null hypothesis of random
#' agreement (i.e., adjusted Rand Index equal to 0) between two partitions.
#'
#' @param ground_truth (int) A vector of the actual membership of elements
#'        in clusters
#' @param partition The partition coming from a clustering algorithm
#'
#' @return A list with six elements:
#'   \item{Rand}{the Rand Index}
#'   \item{ExpectedRand}{expected value of  Rand Index}
#'   \item{AdjustedRand}{Adjusted Rand Index}
#'   \item{var_ari}{variance of Rand Index}
#'   \item{nari}{nari}
#'   \item{p-value}{the p-value of the test}
#'
#' @export
#'
#' @examples
#' library(CrossClustering)
#'
#' clusters <- iris[-5] |>
#'   dist() |>
#'   hclust(method = 'ward.D') |>
#'   cutree(k = 3)
#'
#' ground_truth <- iris[[5]] |>
#'   as.numeric()
#'
#' cc_test_ari(ground_truth, clusters)
#'
#' @author
#' Paola Tellaroli, <paola `dot` tellaroli `at` unipd `dot` it>;
#' Philippe Courcoux, <philippe `dot` courcoux `at` oniris-nantes `dot` fr>
#'
#' @references
#' E_M. Qannari, p. Courcoux and Faye p. (2014) Significance test of the
#' adjusted Rand index. Application to the free sorting task, Food Quality
#' and Preference, (32)93-97
#'
#' L. Hubert and p. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.
cc_test_ari <- function(ground_truth, partition) {

  checkmate::qassert(ground_truth, "N+")
  checkmate::qassert(partition, "N+")
  checkmate::assert_true(length(ground_truth) == length(partition))

  nitem <- length(ground_truth)

  tbl_ground_partition <- table(ground_truth, partition)
  nt    <- rowSums(tbl_ground_partition)
  pt    <- colSums(tbl_ground_partition)

  t <- nitem * (nitem - 1) / 2
  p <- sum(nt^2) - nitem
  q <- sum(pt^2) - nitem

  p_prime <- sum(nt * (nt - 1) * (nt - 2))
  q_prime <- sum(pt * (pt - 1) * (pt - 2))
  q - sum(pt^2) - nitem

  var_b   <- (1 / t) + (
    4 * p_prime * q_prime /
      (nitem * (nitem - 1) * (nitem - 2) * p * q)
  ) + (
    (p - 2 - 4 * (p_prime / p)) *
      (q - 2 - 4 * (q_prime / q)) /
      (nitem * (nitem - 1) * (nitem - 2) * (nitem - 3))
  ) - (
    p * q / (nitem^2 * (nitem - 1)^2)
  )

  var_ri  <- 4 * p * q * var_b / (nitem^2 * (nitem - 1)^2)

  exp_r   <- 1 - ((p + q) / (2 * t)) + ((p * q) / (2 * t^2))

  var_ari <- var_ri * (1 / ((1 - exp_r)^2))

  tt <- sum(tbl_ground_partition^2) - nitem
  r <- (tt - (p / 2) - (q / 2) + t) / t

  rand_adjusted <- (r - exp_r) / (1 - exp_r)

  nari <- rand_adjusted / sqrt(var_ari)
  p_val_nari <- 1 - pnorm(q = nari)

  list(
    rand = r,
    expected_rand = exp_r,
    adjusted_rand = rand_adjusted,
    var_ari = var_ari,
    nari = nari,
    p_value = p_val_nari
  )
}
