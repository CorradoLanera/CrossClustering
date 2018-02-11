#' A permutation test for testing the null hypothesis of
#' random agreement (i.e., adjusted Rand Index equal to 0) between
#' two partitions.
#'
#' @param ground_truth [int] A vector of the actual membership of elements
#'   in clusters
#' @param partition The partition coming from a clustering algorithm
#'
#' @return A data.frame with two columns:
#'   \item{Stat}{the adjusted Rand Index}
#'   \item{p-value}{the p-value of the test}
#'
#' @export
#'
#' @examples
#' library(CrossClustering)
#'
#' clusters <- iris[-5] %>%
#'   dist %>%
#'   hclust(method = 'ward.D') %>%
#'   cutree(k = 3)
#'
#' ground_truth <- iris[[5]] %>% as.numeric()
#'
#' PermSignificanceARI(ground_truth, clusters)
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;
#' Livio Finos, <livio [dot] finos [at] unipd [dot] it>
#'
#' @references
#' Samuh M. H., Leisch F., and Finos L. (2014), Tests for Random Agreement
#' in Cluster Analysis, Statistica Applicata-Italian Journal of Applied
#' Statistics, vol. 26, no. 3, pp. 219-234.
#'
#' L. Hubert and P. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.

PermSignificanceARI <- function(ground_truth, partition) {

  assertive::assert_is_numeric(ground_truth)
  assertive::assert_is_numeric(partition)
  assertive::assert_are_same_length(ground_truth, partition)
  assertive::assert_all_are_not_na(ground_truth)
  assertive::assert_all_are_not_na(partition)

  ari_fixed_partition <- function(ground_truth){
    mclust::adjustedRandIndex(ground_truth, partition)
  }

  # flip force to cut information of no interest for us
  capture.output(
    res_flip <- flip::flip(Y = matrix(ground_truth), X = matrix(partition),
      statTest = ari_fixed_partition
    )
  )

  # original rownames has to be restored, i.e. numeric and not characted
  res <- res_flip@res[c('Stat', 'p-value')]
  rownames(res) <- NULL
  res
}

