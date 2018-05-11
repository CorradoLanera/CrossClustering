#' Get clusters which reach max consensus
#'
#' Computes the consensus between Ward's minimum variance and
#' Complete-linkage (or Single-linkage) algorithms (i.e., the number of
#' elements classified together by both algorithms).
#'
#' @param k [int] a vector containing the number of clusters for Ward and
#'        for Complete-linkage (or Single-linkage) algorithms, respectively
#' @param cluster_ward an object of class hclust for the Ward algorithm
#' @param cluster_other an object of class hclust for the
#'        Complete-linkage (or Single-linkage) algorithm
#' @param return_list [lgl] If TRUE the list of the elements belonging to
#'        each cluster and the contingency table of the clustering are
#'        shown. Otherwise, a numeric vector of lenght one reporting the
#'        number of element classified.
#'
#' @return If return_list is FALSE (default) the number of elements
#'         classified.
#'         If return_list is TRUE, a list with the following elements:
#'           \item{beta_list}{
#'             list of the elements belonging to each cluster
#'           };
#'           \item{A_star}{contingency table of the clustering}.
#'
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
#' ### Hierarchical clustering
#' cluster_ward    <- toy_dist %>% hclust(method = "ward.D")
#' cluster_other <- toy_dist %>% hclust(method = "complete")
#'
#'
#' ### cc_get_max_consensus
#' CrossClustering:::cc_get_max_consensus(c(3, 4),
#'   cluster_ward,
#'   cluster_other
#' )
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

cc_get_max_consensus <- function(k,
  cluster_ward,
  cluster_other,
  return_list = FALSE
) {
  assertive::assert_is_of_length(k, 2)
  assertive::assert_is_numeric(k)
  assertive::assert_is_any_of(cluster_ward, 'hclust')
  assertive::assert_is_any_of(cluster_other, 'hclust')
  assertive::assert_is_a_bool(return_list)

  k_w = k[1]
  k_c = k[2]
  tree_ward     <- cutree(cluster_ward    , k = k_w)
  tree.complete <- cutree(cluster_other , k = k_c)
  N <- sum(tree_ward * 0 + 1)
  A <- table(tree_ward, tree.complete)
  A_star <- diag(0, k_w)

  if(return_list) {
    beta_list <- vector('list', length = length(k_w))
  }

  for(i in seq_len(k_w)) {
    A_star[i, i] <- max(A)
    A_max        <- which(A == max(A), arr.ind = TRUE)[1, ]
    r_max        <- A_max[1]
    c_max        <- A_max[2]

    if(return_list) {
      beta_list[[i]] <- (seq_len(N))[(tree_ward     == r_max) &
                                     (tree.complete == c_max)
                        ]
    }
    A[r_max,      ] <- 0
    A[     , c_max] <- 0
  }

  A_star <- prune_zero_tail(A_star)

  if(return_list) {
    n_clusters <- length(diag(A_star))
    beta_list  <- beta_list[seq_len(n_clusters)] %>%
      stats::setNames(paste("cluster", seq_along(.), sep = "_"))

    return(list(
      "beta_list" = beta_list,
      "A_star"    = A_star
    ))
  }

  as.integer(sum(A_star))
}
