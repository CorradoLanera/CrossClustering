#' CrossClusteringSingle: a partial clustering algorithm with automatic estimation of the number of clusters and identification of outliers for finding elongated clusters
#'
#' This function performs a modified CrossClustering algorithm, tailored to
#' identify elongated clusters. This method combines the Ward's minimum variance
#' and Single-linkage algorithms, providing automatic estimation of a suitable
#' number of elongated clusters and identification of outlier elements.
#'
#' @param d a dissimilarity structure as produced by the function \code{dist}
#' @param k.w.min minimum number of clusters for the Ward's minimum variance
#'        method. By default is set equal 2
#' @param k.w.max maximum number of clusters for the Ward's minimum variance
#'        method (see details)
#' @param k.s.max maximum number of clusters for the Single-linkage method.
#'        It can not be equal or greater than the number of elements to cluster
#'        (see details)
#' @param out logical. If \code{TRUE} (default) outliers must be searched (see
#'        details)
#'
#' @return A list of objects describing characteristics of the partitioning as
#'         follows:
#' \item{Optimal.cluster}{number of clusters}
#' \item{Cluster.list}{a list of clusters; each element of this lists contains
#'   the indices of the elemenents belonging to the cluster}
#' \item{Silhouette}{the average silhouette witdh over all the clusters}
#' \item{n.total}{total number of input elements}
#' \item{n.clustered}{number of input elements that have actually been
#'   clustered}
#'
#' @export
#'
#' @details See cited document for more details.
#' @examples
#'
#' ### Example on a famous shape data set
#' ### Two moons data
#' data(twomoons)
#' plot(twomoons[, 1:2], pch = 19, col = "cornflowerblue")
#' d <- dist(twomoons[, 1:2], method = "euclidean")
#' CCmoons <- CrossClusteringSingle(d, k.w.max = 9, k.s.max = 10)
#'
#' my_col <- sapply(1:dim(twomoons)[1], geneinlista,
#'   lista = CCmoons$Cluster.list
#' )
#' my_col[my_col == "integer(0)"] <- 0
#' my_col <- unlist(my_col)
#' my_col <- my_col + 1
#' plot(twomoons[, 1:2], pch  = 19, col = my_col, xlab = "", ylab = "",
#'   main = "CrossClusteringSingle", cex.main = 1
#' )

#' ### Worms data
#' data(worms)
#' plot(worms[, 1:2], pch = 19, col = "cornflowerblue")
#' d <- dist(worms[, 1:2], method = "euclidean")
#' CCworms <- CrossClusteringSingle(d, k.w.max = 9, k.s.max = 10)
#'
#' my_col <- sapply(1:dim(worms)[1], geneinlista, lista = CCworms$Cluster.list)
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
#' CCchain_effect <- CrossClusteringSingle(d, k.w.max = 9, k.s.max = 10)
#'
#' my_col <- sapply(1:dim(chain_effect)[1], geneinlista,
#'   lista = CCchain_effect$Cluster.list
#' )
#' my_col[my_col == "integer(0)"] <- 0
#' my_col <- unlist(my_col)
#' my_col <- my_col + 1
#' plot(chain_effect, pch = 19, col = my_col, xlab = "",ylab = "",
#'   main = "CrossClusteringSingle", cex.main = 1
#' )
#'
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#' Marco Bazzi, <bazzi [at] stat [dot] unipd [dot] it>;
#' Michele Donato, <mdonato [at] stanford [dot] edu>
#'
#' @references
#' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2017).
#' E1829: Cross-Clustering: A Partial Clustering Algorithm with Automatic
#' Estimation of the Number of Clusters. CMStatistics 2017, London 16-18
#' December, Book of Abstracts (ISBN 978-9963-2227-4-2)

CrossClusteringSingle <- function(d, k.w.min = 2, k.w.max, k.s.max, out = TRUE)
{
  n <- (1 + sqrt(1 + 8 * length(d))) / 2

  beta.clu.ward     <- stats::hclust(d, method = "ward.D")
  beta.clu.single   <- stats::hclust(d, method = "single")

    grid <- as.matrix(expand.grid(k.w.min:k.w.max, k.w.min:k.s.max))

  if (out) {
    grid <- grid[grid[, 2] >  grid[, 1], ]
  } else {
    grid <- grid[grid[, 2] >= grid[, 1], ]
  }
  grid <- cbind(grid, 0)
  colnames(grid) <- c("Ward", "Single", "N. classified")

  n.clu <- vector('list', length = nrow(grid))

  for(i in seq_len(nrow(grid))) {
    n.clu[[i]] <- max_proportion_function(grid[i, ],
      beta.clu.ward     = beta.clu.ward,
      beta.clu.complete = beta.clu.single
    )
  }

  grid[, 3] <- unlist(n.clu)

  grid.star <- which(grid == max(grid[, 3]), arr.ind = TRUE)[, 1]
  k.star    <- rbind(grid[grid.star, 1:2])

  if(is.null(dim(k.star))){
    cluster.list <- max_proportion_function(k.star,
      beta.clu.ward     = beta.clu.ward,
      beta.clu.complete = beta.clu.single,
      return.list       = TRUE
    )
    clustz <- sapply(seq_len(n), geneinlista, lista = cluster.list$beta.list)
  } else {
    cluster.list <- apply(k.star, 1, max_proportion_function,
      beta.clu.ward     = beta.clu.ward,
      beta.clu.complete = beta.clu.single,
      return.list       = TRUE
    )
    clustz <- sapply(cluster.list,
      function(lasim) sapply(seq_len(n), geneinlista, lista = lasim$beta.list)
    )
  }

  clustz[clustz == "integer(0)"] <- 0

  if(is.null(dim(clustz))) {
    clustz <- matrix(clustz, ncol = 1)
  }

  Sil <- vector('list', length = ncol(clustz))

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

  k.c.max <- k.s.max
  beta.clu.complete <- beta.clu.single

  list("Optimal.cluster" = length(cluster.list[[which.max(Sil)]]$beta.list),
       "Cluster.list"    = Cluster.list,
       #       "A.star"          = cluster.list[[which.max(Sil)]]$A.star,
       "Silhouette"      = max(unlist(Sil)),
       "n.total"         = n,
       "n.clustered"     = n.clustered)
}
