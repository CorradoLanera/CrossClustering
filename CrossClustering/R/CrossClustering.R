#' A partial clustering algorithm with automatic estimation of
#' the number of clusters and identification of outliers
#'
#' This function performs the CrossClustering algorithm. This method combines
#' the Ward's minimum variance and Complete-linkage (default, useful for finding
#' spherical clusters) or Single-linkage (useful for finding elongated clusters)
#' algorithms, providing automatic estimation of a suitable number of clusters
#' and identification of outlier elements.
#'
#' @param d A dissimilarity structure as produced by the function \code{dist}
#' @param k.w.min [int] Minimum number of clusters for the Ward's minimum
#'        variance method. By default is set equal 2
#' @param k.w.max [int] Maximum number of clusters for the Ward's minimum
#'        variance method (see details)
#' @param k2.max [int] Maximum number of clusters for the
#'        Complete/Single-linkage method. It can not be equal or greater than
#'        the number of elements to cluster (see details)
#' @param out [lgl] If \code{TRUE} (default) outliers must be searched (see
#'        details)
#' @param method [chr] "complete" (default) or "single". CrossClustering
#'        combines Ward's algorithm with Complete-linkage if method is set to
#'        "complete", otherwhise (if method is set to 'single') Single-linkage
#'        will be used.
#' @return A list of objects describing characteristics of the partitioning as
#'         follows:
#'           \item{Optimal.cluster}{number of clusters}
#'           \item{Cluster.list}{a list of clusters; each element of this lists
#'             contains the indices of the elemenents belonging to the cluster}
#'           \item{Silhouette}{the average silhouette witdh over all the
#'             clusters}
#'           \item{n.total}{total number of input elements}
#'           \item{n.clustered}{number of input elements that have actually been
#'             clustered}
#'
#' @export
#'
#' @details See cited document for more details.
#' @examples
#' library(CrossClustering)
#'
#' #### method = "complete"
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
#' CrossClustering(d, k.w.min = 2, k.w.max = 5, k2.max = 6,
#' out = TRUE)
#'
#'
#' #### method = "single"
#' ### Example on a famous shape data set
#' ### Two moons data
#' data(twomoons)
#' plot(twomoons[, 1:2], pch = 19, col = "cornflowerblue")
#' d <- dist(twomoons[, 1:2], method = "euclidean")
#' CCmoons <- CrossClustering(d, k.w.max = 9, k2.max = 10, method = 'single')
#'
#' my_col <- sapply(1:dim(twomoons)[1], which_cluster,
#'   cluster_list = CCmoons$Cluster.list
#' )
#' my_col[my_col == "integer(0)"] <- 0
#' my_col <- unlist(my_col)
#' my_col <- my_col + 1
#' plot(twomoons[, 1:2], pch  = 19, col = my_col, xlab = "", ylab = "",
#'   main = "CrossClusteringSingle", cex.main = 1
#' )
#'
#' ### Worms data
#' data(worms)
#' plot(worms[, 1:2], pch = 19, col = "cornflowerblue")
#' d <- dist(worms[, 1:2], method = "euclidean")
#' CCworms <- CrossClustering(d, k.w.max = 9, k2.max = 10, method = 'single')
#'
#' my_col <- sapply(seq_len(dim(worms)[1]), which_cluster,
#'   cluster_list = CCworms$Cluster.list
#' )
#' my_col[my_col == "integer(0)"] <- 0
#' my_col <- unlist(my_col)
#' my_col <- my_col + 1
#' plot(worms[, 1:2], pch = 19, col = my_col, xlab = "", ylab = "",
#'   main = "CrossClusteringSingle", cex.main = 1
#' )
#'
#' ### CrossClusteringSingle is not affected to chain-effect problem
#' data(chain_effect)
#' plot(chain_effect, pch = 19, col = "cornflowerblue")
#' d <- dist(chain_effect, method = "euclidean")
#' CCchain_effect <- CrossClustering(d, k.w.max = 9, k2.max = 10,
#'   method = 'single'
#' )
#'
#' my_col <- sapply(1:dim(chain_effect)[1], which_cluster,
#'   cluster_list = CCchain_effect$Cluster.list
#' )
#' my_col[my_col == "integer(0)"] <- 0
#' my_col <- unlist(my_col)
#' my_col <- my_col + 1
#' plot(chain_effect, pch = 19, col = my_col, xlab = "",ylab = "",
#'   main = "CrossClusteringSingle", cex.main = 1
#' )
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#' Marco Bazzi, <bazzi [at] stat [dot] unipd [dot] it>;
#' Michele Donato, <mdonato [at] stanford [dot] edu>
#'
#' @references
#' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2016).
#' Cross-Clustering: A Partial Clustering Algorithm with Automatic Estimation of
#' the Number of Clusters. PLoS ONE 11(3):   e0152333.
#' doi:10.1371/journal.pone.0152333
#'
#' #' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2017).
#' E1829: Cross-Clustering: A Partial Clustering Algorithm with Automatic
#' Estimation of the Number of Clusters. CMStatistics 2017, London 16-18
#' December, Book of Abstracts (ISBN 978-9963-2227-4-2)


CrossClustering <- function(d, k.w.min = 2, k.w.max, k2.max, out = TRUE, method = c('complete', 'single'))
{
  method <- match.arg(method)
  n <- (1 + sqrt(1 + 8 * length(d))) / 2

  beta.clu.ward     <- hclust(d, method = "ward.D")
  beta.clu.method2 <- hclust(d, method = method)

  grid <- as.matrix(expand.grid(k.w.min:k.w.max, k.w.min:k2.max))

  if (out) {
    grid <- grid[grid[, 2] >  grid[, 1], ]
  } else {
    grid <- grid[grid[, 2] >= grid[, 1], ]
  }
  grid <- cbind(grid, 0)
  colnames(grid) <- c("Ward", method, "N. classified")

  n.clu <- vector('list', length = nrow(grid))

  for(i in seq_len(nrow(grid))) {
    n.clu[[i]] <- max_proportion_function(grid[i, ],
      beta.clu.ward     = beta.clu.ward,
      beta.clu.method2 = beta.clu.method2
    )
  }

  grid[, 3] <- unlist(n.clu)

  grid.star <- which(grid == max(grid[, 3]), arr.ind = TRUE)[, 1]
  k.star    <- rbind(grid[grid.star, 1:2])

  if(is.null(dim(k.star))){
    cluster.list <- max_proportion_function(k.star,
      beta.clu.ward     = beta.clu.ward,
      beta.clu.method2 = beta.clu.method2,
      return.list       = TRUE
    )
    clustz <- sapply(seq_len(n), which_cluster,
      cluster_list = cluster.list$beta.list
    )
  } else {
    cluster.list <- apply(k.star, 1, max_proportion_function,
      beta.clu.ward     = beta.clu.ward,
      beta.clu.method2 = beta.clu.method2,
      return.list       = TRUE
    )
    clustz <- sapply(cluster.list,
      function(lasim) sapply(seq_len(n), which_cluster,
        cluster_list = lasim$beta.list
      )
    )
  }

  clustz[clustz == "integer(0)"] <- 0

  if(is.null(dim(clustz))){
    clustz <- matrix(clustz, ncol = 1)
  }

  Sil <- vector('double', length = ncol(clustz))


  for (c in seq_len(ncol(clustz))) {
    Sil[[c]] <- mean(
      cluster::silhouette(as.numeric(clustz[, c]), dist = d)[, 3]
    )
  }

  if(is.null(dim(k.star))) {
    k.star.star <- k.star[which.max(Sil)]
  } else {
    k.star.star <- k.star[which.max(Sil), ]
  }

  Cluster.list <- cluster.list[[which.max(Sil)]]$beta.list
  n.clustered  <- length(unlist(Cluster.list))

  list("Optimal.cluster" = length(cluster.list[[which.max(Sil)]]$beta.list),
       "Cluster.list"    = Cluster.list,
 #       "A.star"          = cluster.list[[which.max(Sil)]]$A.star,
       "Silhouette"      = max(unlist(Sil)),
       "n.total"         = n,
       "n.clustered"     = n.clustered)
}
