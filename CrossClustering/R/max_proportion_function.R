#' Computes the consensus between Ward's minimum
#' variance and Complete-linkage (or Single-linkage) algorithms (i.e., the
#' number of elements classified together by both algorithms) .
#'
#' @param k [int] a vector containing the number of clusters for Ward and for
#'        Complete-linkage (or Single-linkage) algorithms, respectively
#' @param beta.clu.ward an object of class hclust for the Ward algorithm
#' @param beta.clu.method2 an object of class hclust for the Complete-linkage
#'        (or Single-linkage) algorithm
#' @param return.list [lgl] If TRUE the list of the elements belonging to each
#'        cluster and the contingency table of the clustering are shown.
#'
#' @return If return.list is FALSE (default) the number of elements classified.
#'
#'         If return.list is TRUE, a list with the following elements:
#'           \item{beta.list}{list of the elements belonging to each cluster};
#'           \item{A.star}{contingency table of the clustering}.
#'
#'
#' @examples
#' library(CrossClustering)
#'
#' ### Generate simulated data
#' toy <- matrix(NA, nrow = 10, ncol = 7)
#' colnames(toy) <- paste("Sample", 1:ncol(toy), sep = "")
#' rownames(toy) <- paste("Gene"  , 1:nrow(toy), sep = "")
#' set.seed(123)
#'
#' toy[, 1:2] <- rnorm(n = nrow(toy) * 2, mean = 10, sd  = 0.1)
#' toy[, 3:4] <- rnorm(n = nrow(toy) * 2, mean = 20, sd  = 0.1)
#' toy[, 5:6] <- rnorm(n = nrow(toy) * 2, mean = 5 , sd  = 0.1)
#' toy[,   7] <- runif(n = nrow(toy)    , min  = 0 , max = 1  )
#'
#' ### toy is transposed as we want to cluster samples (columns of the original
#' ### matrix)
#' d <- dist(t(toy), method = "euclidean")
#'
#' ### Hierarchical clustering
#' beta.clu.ward     <- hclust(d, method = "ward.D")
#' beta.clu.method2 <- hclust(d, method = "complete")
#'
#' ### max_proportion_function
#' CrossClustering:::max_proportion_function(c(3, 4),
#'   beta.clu.ward,
#'   beta.clu.method2
#' )
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#' Marco Bazzi, <bazzi [at] stat [dot] unipd [dot] it>;
#' Michele Donato, <mdonato [at] stanford [dot] edu>.
#'
#' @references
#' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2016).
#' Cross-Clustering: A Partial Clustering Algorithm with Automatic Estimation
#' of the Number of Clusters. PLoS ONE 11(3):   e0152333.
#' doi:10.1371/journal.pone.0152333

max_proportion_function <- function(
  k, beta.clu.ward, beta.clu.method2, return.list = FALSE
) {
  k.w = k[1]
  k.c = k[2]
  tree.ward     <- cutree(beta.clu.ward    , k = k.w)
  tree.complete <- cutree(beta.clu.method2, k = k.c)
  N <- sum(tree.ward * 0 + 1)
  A <- table(tree.ward, tree.complete)
  A.star <- diag(0, k.w)

  if(return.list) {
    beta.list <- vector('list', length = length(k.w))
  }

  # i <- 1
  for(i in seq_len(k.w)) {
    A.star[i, i] <- max(A)
    A.max        <- which(A == max(A), arr.ind = TRUE)[1, ]
    r.max        <- A.max[1]
    c.max        <- A.max[2]

    if(return.list) {
      beta.list[[i]] <- (seq_len(N))[(tree.ward     == r.max) &
                                     (tree.complete == c.max)
                        ]
    }
    A[r.max,      ] <- 0
    A[     , c.max] <- 0
  }

  while (A.star[nrow(A.star), ncol(A.star)] == 0) {
    A.star <- A.star[-(nrow(A.star)), -(ncol(A.star))]

    if(return.list) {
      beta.list <- beta.list[-length(beta.list)]
    }

    if(is.null(dim(A.star))) break
  }

  if(return.list) {
    return(list("beta.list" = beta.list, "A.star" = A.star))
  }

  sum(A.star)
}
