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
#' A vector of objects:
#' \item{AdjustedRandIndex}{The adjusted Rand Index}
#' \item{CI}{The confidence interval}
#'
#' @export
#'
#' @examples
#'
#' #### This example compares the adjusted Rand Index as computed on the
#' ### partitions given by Ward's algorithm with the ground truth on the famous
#' ### Iris data set by the adjustedRandIndex function {mclust package} and by
#' ### the ARI_contingency function
#'
#' clusters <- iris[-5] %>%
#'   dist %>%
#'   hclust(method = 'ward.D') %>%
#'   cutree(k = 3)
#'
#' ground_truth <- iris[,5] %>%
#' as.numeric()
#'
#' mc_ari <- mclust:::adjustedRandIndex(clusters, ground_truth)
#' mc_ari
#'
#' mat <- table(ground_truth, clusters)
#' cc_ari <- ARI_contingency(mat, digits = 7)
#' ari_cc <- cc_ari['ari'] %>% unname
#' all.equal(mc_ari, ari_cc)
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#'
#' @references
#' L. Hubert and P. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.
#'
#' D. Steinley, M. J. Brusco, L. Hubert (2016) The Variance of the Adjusted
#' Rand Index, Psychological Methods, 21(2), 261-272
#'
#' D. Steinley (2004) Properties of the Hubert-Arabie Adjusted Rand Index,
#' Psychological Methods, 9(3), 386-396

ARI_contingency <- function(mat, alpha = 0.05, digits = 2){
  assertive::assert_is_matrix(mat)
  assertive::assert_is_numeric(mat)
  assertive::assert_all_are_greater_than_or_equal_to(mat, 0)
  assertive::assert_all_are_equal_to(mat, as.integer(mat))
  assertive::assert_is_a_double(alpha)
  assertive::assert_all_are_proportions(alpha,
    lower_is_strict = TRUE,
    upper_is_strict = TRUE
  )
  assertive::assert_is_a_number(digits)
  assertive::assert_all_are_greater_than_or_equal_to(digits, 0)


  n_pox <- choose(sum(mat), 2)
  N     <- sum(mat)
  p     <- 0

  num1 <- cell <- numeric(nrow(mat) * ncol(mat))
  num2 <- numeric(ncol(mat))
  num3 <- numeric(nrow(mat))

  for (i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      p <- p + 1
      num1[[p]] <- choose(mat[i,j],2)
      num2[[j]] <- choose(colSums(mat)[j],2)
      num3[[i]] <- choose(rowSums(mat)[i],2)
      cell[[p]] <- mat[i,j]
    }
  }

  a <- (sum(cell^2) - N) / 2
  b <- (sum(rowSums(mat)^2) - (sum(cell^2))) / 2
  c <- (sum(colSums(mat)^2) - (sum(cell^2))) / 2
  d <- (sum(cell^2) + N^2 - sum(rowSums(mat)^2) - sum(colSums(mat)^2)) / 2

  exp <- sum(num2) * sum(num3) / n_pox
  num <- sum(num1) - exp
  den <- 0.5 * (sum(num2) + sum(num3)) - exp

  e <- 2 * sum(rowSums(mat)^2) -
       (N + 1) * N

  f <- 2 * sum(colSums(mat)^2) -
       (N + 1) * N

  g <- 4 * sum(rowSums(mat)^3) -
       4 * (N + 1) * sum(rowSums(mat)^2) +
       (N + 1)^2 * N

  h <- N * (N - 1)

  i <- 4 * sum(colSums(mat)^3) -
       4 * (N + 1) * sum(colSums(mat)^2) +
       (N + 1)^2 * N

  var_ad <- 1/16 * (
              2 * N * (N - 1) -
              ((e * f) / (N * (N - 1)))^2 +
              (4 * (g - h) * (i - h)) / (N * (N - 1) * (N - 2))
            ) +
            1/16 * (
              ((e^2 - 4 * g + 2 * h) * (f^2 - 4 * i + 2 * h)) /
              (N * (N - 1) * (N - 2) * (N - 3))
            )

  var_ARI <- (n_pox^2 * var_ad) /
             (
               (n_pox^2 - ((a + b) * (a + c) + (b + d) * (c + d)))^2
             )

  ARI <- num/den
  LL <- ARI - stats::qnorm(p = (1 - alpha / 2)) * sqrt(var_ARI)
  UL <- ARI + stats::qnorm(p = (1 - alpha / 2)) * sqrt(var_ARI)

  c(
    "ari"     = round(ARI, digits = digits),
    "ci_ll"  = round(LL,  digits = digits),
    "ci_ul" = round(UL,  digits = digits)
  )
}

