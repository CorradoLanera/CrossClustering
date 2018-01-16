#' PermSignificanceARI: a permutation test for testing the null hypothesis of
#' random agreement (i.e., adjusted Rand Index equal to 0) between
#' two partitions.
#'
#' @param ground_truth The actual membership of elements in clusters
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
#' ### Two moons data
#' data(twomoons)
#' d <- dist(twomoons[,1:2], method = "euclidean")
#' CCmoons <- CrossClusteringSingle(d,k.w.max=9,k.s.max=10)
#' CCmoons_clusters <- sapply(1:dim(twomoons)[1], CrossClustering:::geneinlista,
#' CCmoons$Cluster.list)
#' CCmoons_clusters[CCmoons_clusters == "integer(0)"] <- 0
#' CCmoons_clusters <- unlist(CCmoons_clusters) + 1
#'
#' PermSignificanceARI(twomoons[,3], CCmoons_clusters)
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;
#' Livio Finos, <livio [dot] finos [at] unipd [dot] it>
#'
#' @references
#' Samuh M. H., Leisch F., and Finos L. (2014), Tests for Random Agreement in
#' Cluster Analysis, Statistica Applicata-Italian Journal of Applied Statistics,
#' vol. 26, no. 3, pp. 219-234.
#'
#' L. Hubert and P. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.

PermSignificanceARI <- function(ground_truth, partition) {
  my_fun <- function(ground_truth){
    mclust::adjustedRandIndex(ground_truth, partition)
  }

  tmp <- tempfile()
  sink('tmp');
  res_flip <- flip::flip(Y = matrix(ground_truth), X = matrix(partition),
                         statTest = my_fun
  )
  sink(NULL);
  unlink('tmp')

  res_flip@res[c('Stat', 'p-value')]
}

