#' Provides the vector of clusters' ID to which each element belong to.
#'
#' @param x list of clustered elements or a `crossclustering` object
#' @param n_elem total number of elements clustered (ignored if x
#'        is of class `crossclustering`)
#'
#' @return An integer vector of clusters to which the elements belong (`1`
#'         for the outliers, ID + 1 for the others).
#' @export
cc_get_cluster <- function(x, n_elem) {
  UseMethod("cc_get_cluster", x)
}



#' @export
#'
#' @describeIn cc_get_cluster default method for [cc_get_cluster].
#'
#' @examples
#' library(CrossClustering)
#'
#' data(toy)
#'
#' ### toy is transposed as we want to cluster samples (columns of the
#' ### original matrix)
#' toy_dist <- t(toy) |>
#'   dist(method = "euclidean")
#'
#' ### Run CrossClustering
#' toyres <- cc_crossclustering(
#'   toy_dist,
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
#' Paola Tellaroli, <paola `dot` tellaroli `at` unipd `dot` it>;;
#' Marco Bazzi, <bazzi `at` stat `dot` unipd `dot` it>;
#' Michele Donato, <mdonato `at` stanford `dot` edu>.
#'
#' @references
#' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2016).
#' Cross-Clustering: A Partial Clustering Algorithm with Automatic
#' Estimation of the Number of Clusters. PLoS ONE 11(3):   e0152333.
#' doi:10.1371/journal.pone.0152333
cc_get_cluster.default <- function(x, n_elem) {

  checkmate::qassert(x, "L")
  checkmate::qassert(n_elem, "X1")

  elements  <- unlist(x)

  if (n_elem < length(elements)) {
    stop("n_elem has to be at least the number of clustered elements")
  }

  outliers <- setdiff(seq_len(n_elem), elements)

  clustered_cluster <- vapply(elements,
    function(element) {
      which(vapply(x,
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


#' @describeIn cc_get_cluster automatically extract inputs from a
#'   `crossclustering` object
#'
#' @export
#'
#' @examples
#'
#' ### cc_get_cluster directly from a crossclustering object
#' cc_get_cluster(toyres)
cc_get_cluster.crossclustering <- function(x, n_elem) {
  checkmate::assert_class(x, "crossclustering")
  cc_get_cluster.default(x, attr(x, "n_total"))
}
