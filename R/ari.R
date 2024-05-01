#' Computes the adjusted Rand index and the confidence interval, comparing
#' two classifications from a contingency table.
#'
#' @param mat A matrix of integers representing the contingency table of
#'   reference
#' @param alpha A single number strictly included between 0 and 1
#'   representing the significance level of interest. (default is 0.05)
#' @param digits An integer for the returned significant digits to return
#'   (default is 2)
#'
#' @details
#' The adjusted Rand Index (ARI) should be interpreted as follows:
#'
#' ARI >= 0.90 excellent recovery;
#' 0.80 =< ARI < 0.90 good recovery;
#' 0.65 =< ARI < 0.80 moderate recovery;
#' ARI < 0.65 poor recovery.
#'
#' As the confidence interval is based on the approximation to the Normal
#' distribution, it is recommended to trust in the confidence interval only
#' in cases of total number of object clustered greater than 100.
#'
#' @return
#' An object of class `ari` with the following elements:
#'   \item{AdjustedRandIndex}{The adjusted Rand Index}
#'   \item{CI}{The confidence interval}
#'
#' @export
#'
#' @examples
#'
#' #### This example compares the adjusted Rand Index as computed on the
#' ### partitions given by Ward's algorithm with the ground truth on the
#' ### famous Iris data set by the adjustedRandIndex function
#' ### {mclust package} and by the ari function.
#'
#' library(CrossClustering)
#' library(mclust)
#'
#' clusters <- iris[-5] |>
#'   dist() |>
#'   hclust(method = 'ward.D') |>
#'   cutree(k = 3)
#'
#' ground_truth <- iris[[5]] |> as.numeric()
#'
#' mc_ari <- adjustedRandIndex(clusters, ground_truth)
#' mc_ari
#'
#' ari_cc <- table(ground_truth, clusters) |>
#'   ari(digits = 7)
#' ari_cc
#'
#' all.equal(mc_ari, unclass(ari_cc)[["ari"]], check.attributes = FALSE)
#'
#' @author
#' Paola Tellaroli, <paola `dot` tellaroli `at` unipd `dot` it>;
#'
#' @references
#' L. Hubert and P. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.
#'
#' E.M. Qannari, P. Courcoux and Faye P. (2014) Significance test of the
#' adjusted Rand index. Application to the free sorting task, Food Quality
#' and Preference, (32)93-97
#'
#' M.H. Samuh, F. Leisch, and L. Finos (2014), Tests for Random Agreement
#' in Cluster Analysis, Statistica Applicata-Italian Journal of Applied
#' Statistics, vol. 26, no. 3, pp. 219-234.
#'
#' D. Steinley (2004) Properties of the Hubert-Arabie Adjusted Rand Index,
#' Psychological Methods, 9(3), 386-396
#'
#' D. Steinley, M.J. Brusco, L. Hubert (2016) The Variance of the Adjusted
#' Rand Index, Psychological Methods, 21(2), 261-272

ari <- function(mat, alpha = 0.05, digits = 2) {
  checkmate::assert_matrix(mat)
  checkmate::qassert(mat, "M*[0,)")
  checkmate::qassert(mat, "X*[0,)")
  checkmate::qassert(alpha, "R1(0,1)")
  checkmate::qassert(digits, "X1[0,)")

  if (sum(mat) < 100) warning(paste0("\n",
    "Sum of elements of ", crayon::blue("mat"), " is less than 100,\n",
    "  confidence interval should not be trusted."
  ))

  n_pox <- choose(sum(mat), 2)
  n <- sum(mat)
  p <- 0

  num1 <- cell <- numeric(nrow(mat) * ncol(mat))
  num2 <- numeric(ncol(mat))
  num3 <- numeric(nrow(mat))

  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      p <- p + 1
      num1[[p]] <- choose(mat[i, j], 2)
      num2[[j]] <- choose(colSums(mat)[j], 2)
      num3[[i]] <- choose(rowSums(mat)[i], 2)
      cell[[p]] <- mat[i, j]
    }
  }

  a <- (sum(cell^2) - n) / 2
  b <- (sum(rowSums(mat)^2) - (sum(cell^2))) / 2
  c <- (sum(colSums(mat)^2) - (sum(cell^2))) / 2
  d <- (sum(cell^2) + n^2 - sum(rowSums(mat)^2) - sum(colSums(mat)^2)) / 2

  exp <- sum(num2) * sum(num3) / n_pox
  num <- sum(num1) - exp
  den <- 0.5 * (sum(num2) + sum(num3)) - exp

  e <- 2 * sum(rowSums(mat)^2) - (n + 1) * n

  f <- 2 * sum(colSums(mat)^2) - (n + 1) * n

  g <- 4 * sum(rowSums(mat)^3) -
    4 * (n + 1) * sum(rowSums(mat)^2) +
    (n + 1)^2 * n

  h <- n * (n - 1)

  i <- 4 * sum(colSums(mat)^3) -
    4 * (n + 1) * sum(colSums(mat)^2) +
    (n + 1)^2 * n

  var_ad <- 1 / 16 * (
    2 * n * (n - 1) -
      ((e * f) / (n * (n - 1)))^2 +
      (4 * (g - h) * (i - h)) / (n * (n - 1) * (n - 2))
  ) +
    1 / 16 * (
      ((e^2 - 4 * g + 2 * h) * (f^2 - 4 * i + 2 * h)) /
        (n * (n - 1) * (n - 2) * (n - 3))
    )

  var_ari <- (n_pox^2 * var_ad) /
    ((n_pox^2 - ((a + b) * (a + c) + (b + d) * (c + d)))^2)

  ari   <- num / den
  lower <- ari - stats::qnorm(p = (1 - alpha / 2)) * sqrt(var_ari)
  upper <- ari + stats::qnorm(p = (1 - alpha / 2)) * sqrt(var_ari)

  partitions <- reverse_table(mat)

  structure(
    list(
      ari = structure(
        round(ari, digits = digits),
        ci = c(
          "lower" = round(lower, digits = digits),
          "upper" = round(upper, digits = digits)
        )
      ),
      input = list(
        mat = mat,
        alpha = alpha
      ),
      partitions = partitions,
      p_values = c(
        "test_ari" = cc_test_ari(
          partitions[[1]], partitions[[2]]
        )[["p_value"]],
        "test_ari_permutation" = cc_test_ari_permutation(
          partitions[[1]],
          partitions[[2]]
        )[["p_value"]]
      )
    ),
    class = "ari"
  )
}


#' print method for ari class
#'
#' @inheritParams base::print
#' @describeIn ari
#'
#' @export
print.ari <- function(x, ...) {
  judge_ari <- dplyr::case_when(
    x[["ari"]] >= 0.9  ~ crayon::green("excellent recovery"),
    x[["ari"]] >= 0.8  ~ crayon::blue("good recovery"),
    x[["ari"]] >= 0.65 ~ crayon::blue("moderate recovery"),
    TRUE               ~ crayon::red("poor recovery")
  )

  judge_lower <- attr(x[["ari"]], "ci")[["lower"]]
  judge_lower <- dplyr::case_when(
    judge_lower >= 0.9  ~ crayon::green(judge_lower),
    judge_lower >= 0.8  ~ crayon::blue(judge_lower),
    judge_lower >= 0.65 ~ crayon::blue(judge_lower),
    TRUE                ~ crayon::red(judge_lower)
  )

  judge_upper <- attr(x[["ari"]], "ci")[["upper"]]
  judge_upper <- dplyr::case_when(
    judge_upper >= 0.9  ~ crayon::green(judge_upper),
    judge_upper >= 0.8  ~ crayon::blue(judge_upper),
    judge_upper >= 0.65 ~ crayon::blue(judge_upper),
    TRUE                ~ crayon::red(judge_upper)
  )

  qannari_p <- x[["p_values"]][["test_ari"]]
  qannari_p <- dplyr::case_when(
    qannari_p < 0.001 ~ crayon::green("< 0.001"),
    qannari_p > 0.05  ~ crayon::red(" ", qannari_p),
    TRUE              ~ crayon::blue(" ", qannari_p)
  )

  permutation_p <- x[["p_values"]][["test_ari_permutation"]]
  permutation_p <- dplyr::case_when(
    permutation_p < 0.001 ~ crayon::green("< 0.001"),
    permutation_p > 0.05  ~ crayon::red(" ", permutation_p),
    TRUE                  ~ crayon::blue(" ", permutation_p)
  )

  cli::cat_line(
    "    Adjusted Rand Index (alpha = ",
    crayon::blue(x[["input"]][["alpha"]]), ")",
    "\n"
  )
  cli::cat_line(
    "ARI                  = ", crayon::blue(x[["ari"]]),
    " (", judge_ari, ")"
  )
  cli::cat_line(
    "Confidence interval  = [", judge_lower, ", ", judge_upper, "]"
  )
  cli::cat_line()
  cli::cat_line("p-values:")
  cli::cat_line(
    "  * Qannari test     = ", qannari_p
  )
  cli::cat_line(
    "  * Permutation test = ", permutation_p
  )
}
