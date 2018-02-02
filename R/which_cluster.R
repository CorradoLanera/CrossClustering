#' Provides the vector of clusters' ID to which each element belong to.
#'
#' @param cluster_list list of clustered elements
#' @param n_elem total number of elements clustered
#'
#' @return An integer vector of clusters to which the elements belong (`1`
#'         for the outliers, ID + 1 for the others).
#'
#' @export
#'
#' @examples
#' library(CrossClustering)
#'
#' data(toy)
#'
#' ### toy is transposed as we want to cluster samples (columns of the
#'     original matrix)
#' d <- dist(t(toy), method = "euclidean")
#'
#' ### Run CrossClustering
#' toyres <- CrossClustering(d,
#'   k.w.min = 2,
#'   k.w.max = 5,
#'   k2.max = 6,
#'   out     = TRUE
#' )
#'
#' ### which_cluster
#' which_cluster(toyres$Cluster.list, toyres$n.total)
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#' Marco Bazzi, <bazzi [at] stat [dot] unipd [dot] it>;
#' Michele Donato, <mdonato [at] stanford [dot] edu>.
#'
#' @references
#' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2016).
#' Cross-Clustering: A Partial Clustering Algorithm with Automatic
#' Estimation of the Number of Clusters. PLoS ONE 11(3):   e0152333.
#' doi:10.1371/journal.pone.0152333

which_cluster <- function(cluster_list, n_elem) {

  assertive::assert_is_list(cluster_list)
  assertive::assert_is_a_number(n_elem)

  n_cluster <- length(cluster_list)
  elements  <- unlist(cluster_list)

  if (n_elem < length(elements)) {
    stop('n_elem has to be at least the number of clustered elements')
  }

  outliers <- setdiff(seq_len(n_elem), elements)

  clustered_cluster <- vapply(elements,
    function(element) {
      which(vapply(cluster_list,
        function(cluster) element %in% cluster,
        logical(1L)
      ))
    },
    integer(1L)
  )

  res <- integer(n_elem)
  res[elements] <- clustered_cluster + 1L
  res[outliers] <- 1L
  res
}
