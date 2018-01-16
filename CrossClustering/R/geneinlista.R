#' geneinlista
#'
#' This function - from a list of elements for each cluster - provides the list
#' of clusters to which the elements belong.
#'
#' @param mygene elements to be clustered
#' @param lista list of clustered elements
#'
#' @return  The list of clusters to which the elements belong.
#'
#' @examples
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
#' ### Run CrossClustering
#' toyres <- CrossClustering(d,
#'   k.w.min = 2,
#'   k.w.max = 5,
#'   k.c.max = 6,
#'   out     = TRUE
#' )
#'
#' ### geneinlista
#' sapply(seq_len(ncol(toy)), geneinlista, toyres$Cluster.list)
#'
#' @author
#' Paola Tellaroli, \email{paola.tellaroli@unipd.it};
#' Marco Bazzi, \email{bazzi@stat.unipd.it};
#' Michele Donato, \email{mdonato@stanford.edu}.
#'
#' @references
#' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2016).
#' Cross-Clustering: A Partial Clustering Algorithm with Automatic Estimation
#' of the Number of Clusters. PLoS ONE 11(3):   e0152333.
#' doi:10.1371/journal.pone.0152333

geneinlista <- function(mygene, lista) {
  which(sapply(lista, function(elem) mygene %in% elem))
}
