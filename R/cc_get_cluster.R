#' Provides the vector of clusters' ID to which each element belong to.
#'
#' @param x list of clustered elements or a \code{crossclustering} object
#' @param ... furter input for the function (e.g. total number of elements
#'   clustered if it is not provided a crosscustering object)
#'
#' @return An integer vector of clusters to which the elements belong (`1`
#'         for the outliers, ID + 1 for the others).
#' @export
cc_get_cluster <- function(x, ...) {
  UseMethod('cc_get_cluster', x)
}



#' @param cluster_list list of clustered elements
#' @param n_elem total number of elements clustered
#'
#' @export
#'
#' @describeIn cc_get_cluster default method for \code{\link{cc_get_cluster}}.
#'
#' @examples
#' library(CrossClustering)
#'
#' data(toy)
#'
#' ### toy is transposed as we want to cluster samples (columns of the
#' ### original matrix)
#' toy_dist <- t(toy) %>% dist(method = "euclidean")
#'
#' ### Run CrossClustering
#' toyres <- cc_crossclustering(toy_dist,
#'   k_w_min = 2,
#'   k_w_max = 5,
#'   k2_max  = 6,
#'   out     = TRUE
#' )
#'
#' ### cc_get_cluster
#' cc_get_cluster(toyres[], 7)
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

cc_get_cluster.default <- function(cluster_list, n_elem) {

  assertive::assert_is_list(cluster_list)
  assertive::assert_is_a_number(n_elem)
  assertive::assert_all_are_equal_to(n_elem, as.integer(n_elem))

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


#' @param crossclustering an object of class crossclustering
#'
#' @describeIn cc_get_cluster automatically extract inputs from a \code{crossclustering} object
#'
#' @export
#'
#' @examples
#'
#' ### cc_get_cluster directly from a crossclustering object
#' cc_get_cluster(toyres)
cc_get_cluster.crossclustering <- function(crossclustering) {
  assertive::assert_is_inherited_from(crossclustering, 'crossclustering')
  cc_get_cluster.default(crossclustering, attr(crossclustering, 'n_total'))
}
