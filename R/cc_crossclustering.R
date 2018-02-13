#' A partial clustering algorithm with automatic estimation of
#' the number of clusters and identification of outliers
#'
#' This function performs the CrossClustering algorithm. This method
#' combines the Ward's minimum variance and Complete-linkage (default,
#' useful for finding spherical clusters) or Single-linkage (useful for
#' finding elongated clusters) algorithms, providing automatic estimation of
#' a suitable number of clusters and identification of outlier elements.
#'
#' @param d A dissimilarity structure as produced by the function
#'        \code{dist}
#' @param k.w.min [int] Minimum number of clusters for the Ward's minimum
#'        variance method. By default is set equal 2
#' @param k.w.max [int] Maximum number of clusters for the Ward's minimum
#'        variance method (see details)
#' @param k2.max [int] Maximum number of clusters for the
#'        Complete/Single-linkage method. It can not be equal or greater
#'        than the number of elements to cluster (see details)
#' @param out [lgl] If \code{TRUE} (default) outliers must be searched (see
#'        details)
#' @param method [chr] "complete" (default) or "single". CrossClustering
#'        combines Ward's algorithm with Complete-linkage if method is set
#'        to "complete", otherwhise (if method is set to 'single')
#'        Single-linkage will be used.
#' @return A list of objects describing characteristics of the partitioning
#'         as follows:
#'           \item{Optimal.cluster}{number of clusters}
#'           \item{Cluster.list}{a list of clusters; each element of this
#'           lists contains the indices of the elemenents belonging to the
#'           cluster}
#'           \item{Silhouette}{the average silhouette witdh over all the
#'             clusters}
#'           \item{n.total}{total number of input elements}
#'           \item{n.clustered}{number of input elements that have actually
#'             been clustered}
#'
#' @export
#'
#' @details See cited document for more details.
#' @examples
#' library(CrossClustering)
#'
#' #### Example of Cross-Clustering as in reference paper
#' #### method = "complete"
#'
#' data(toy)
#'
#' ### toy is transposed as we want to cluster samples (columns of the
#' ### original matrix)
#' d <- t(toy) %>%
#'   dist(method = "euclidean")
#'
#' ### Run CrossClustering
#' cc_crossclustering(d, k.w.min = 2, k.w.max = 5, k2.max = 6, out = TRUE)
#'
#' #### Simulated data as in reference paper
#' #### method = "complete"
#' set.seed(10)
#' sg <- c(500, 250, 700, 300, 100)
#'
#' # 5 clusters
#'
#' t <- matrix(0, nrow = 5, ncol = 5)
#' t[1, ] <- rep(6, 5)
#' t[2, ] <- c( 0,  5, 12, 13, 15)
#' t[3, ] <- c(15, 11,  9,  5,  0)
#' t[4, ] <- c( 6, 12, 15, 10,  5)
#' t[5, ] <- c(12, 17,  3,  7, 10)
#'
#' t_mat <- NULL
#' for (i in seq_len(nrow(t))){
#'   t_mat <- rbind(
#'     t_mat,
#'     matrix(rep(t[i, ], sg[i]), nrow = sg[i], byrow = TRUE)
#'   )
#' }
#'
#' data_15 <- matrix(NA, nrow = 2000, ncol = 5)
#' data_15[1:1850, ] <- matrix(abs(rnorm(sum(sg) * 5, sd = 1.5)),
#'   nrow = sum(sg),
#'   ncol = 5
#' ) + t_mat
#'
#' set.seed(100) # simulate outliers
#' data_15[1851:2000, ] <- matrix(
#'   runif(n = 150 * 5, min = 0, max = max(data_15, na.rm = TRUE)),
#'   nrow = 150,
#'   ncol = 5
#' )
#'
#' ### Run CrossClustering
#' cc_crossclustering(dist(data_15),
#'   k.w.min = 2,
#'   k.w.max = 19,
#'   k2.max  = 20,
#'   out     = TRUE
#' )
#'
#'
#'
#'
#'
#'
#' #### method = "single"
#' ### Example on a famous shape data set
#' ### Two moons data
#'
#' data(twomoons)
#'
#' moons_dist <- twomoons[, 1:2] %>%
#'   dist(method = "euclidean")
#'
#' cc_moons <- cc_crossclustering(moons_dist,
#'   k.w.max = 9,
#'   k2.max  = 10,
#'   method  = 'single'
#' )
#'
#' moons_col <- cc_get_cluster(cc_moons$Cluster.list, cc_moons$n.total)
#' plot(twomoons[, 1:2], col = moons_col,
#'   pch      = 19,
#'   xlab     = "",
#'   ylab     = "",
#'   main     = "CrossClustering-Single"
#' )
#'
#' ### Worms data
#' data(worms)
#'
#' worms_dist <- worms[, 1:2] %>%
#'   dist(method = "euclidean")
#'
#' cc_worms <- cc_crossclustering(worms_dist,
#'   k.w.max = 9,
#'   k2.max  = 10,
#'   method  = 'single'
#' )
#'
#' worms_col <-  cc_get_cluster(cc_worms$Cluster.list, cc_worms$n.total)
#'
#' plot(worms[, 1:2], col = worms_col,
#'   pch      = 19,
#'   xlab     = "",
#'   ylab     = "",
#'   main     = "CrossClustering-Single"
#' )
#'
#'
#' ### CrossClustering-Single is not affected to chain-effect problem
#'
#' data(chain_effect)
#'
#' chain_dist <- chain_effect %>%
#'   dist(method = "euclidean")

#' cc_chain <- cc_crossclustering(chain_dist,
#'   k.w.max = 9,
#'   k2.max  = 10,
#'   method  = 'single'
#' )
#'
#' chain_col <- cc_get_cluster(cc_chain$Cluster.list, cc_chain$n.total)
#'
#' plot(chain_effect, col = chain_col,
#'   pch  = 19,
#'   xlab = "",
#'   ylab = "",
#'   main = "CrossClustering-Single"
#' )
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#' Marco Bazzi, <bazzi [at] stat [dot] unipd [dot] it>;
#' Michele Donato, <mdonato [at] stanford [dot] edu>
#'
#' @references
#' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2016).
#' Cross-Clustering: A Partial Clustering Algorithm with Automatic
#' Estimation of the Number of Clusters. PLoS ONE 11(3):   e0152333.
#' doi:10.1371/journal.pone.0152333
#'
#' #' Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2017).
#' E1829: Cross-Clustering: A Partial Clustering Algorithm with Automatic
#' Estimation of the Number of Clusters. CMStatistics 2017, London 16-18
#' December, Book of Abstracts (ISBN 978-9963-2227-4-2)


cc_crossclustering <- function(d,
  k.w.min = 2,
  k.w.max = attr(d, 'Size') - 2,
  k2.max  = k.w.max + 1,
  out     = TRUE,
  method  = c('complete', 'single')
) {
  # imput check
  assertive::assert_is_any_of(d, 'dist')
  assertive::assert_is_a_number(k.w.min)

  assertive::assert_all_are_positive(k.w.min)
  if (!assertive::assert_all_are_equal_to(k.w.min, as.integer(k.w.min))) {
    stop(paste0('k.w.min must be an integer. It is: ', k.w.min, '.'))
  }

  assertive::assert_is_a_number(k.w.max)
  assertive::assert_all_are_less_than(k.w.min, k.w.max)
  if (!assertive::assert_all_are_equal_to(k.w.max, as.integer(k.w.max))) {
    stop(paste0('k.w.max must be an integer. It is: ', k.w.max, '.'))
  }

  assertive::assert_is_a_number(k2.max)
  assertive::assert_all_are_less_than(k.w.min, k2.max)
  if (!assertive::assert_all_are_equal_to(k2.max, as.integer(k2.max))) {
    stop(paste0('k2.max must be an integer. It is: ', k2.max, '.'))
  }

  assertive::assert_is_a_bool(out)
  assertive::assert_is_character(method)

  method <- match.arg(method)

    # n <- (1 + sqrt(1 + 8 * length(d))) / 2 # this is not an integer!!!!
  n <- attr(d, 'Size')

  beta.clu.ward    <- hclust(d, method = "ward.D")
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
    n.clu[[i]] <- cc_max_proportion(grid[i, c(1, 2)],
      beta.clu.ward     = beta.clu.ward,
      beta.clu.method2  = beta.clu.method2
    )
  }

  grid[, 3] <- unlist(n.clu)

  grid.star <- which(grid == max(grid[, 3]), arr.ind = TRUE)[, 1]
  k.star    <- rbind(grid[grid.star, 1:2])

  if(is.null(dim(k.star))){
    cluster.list <- cc_max_proportion(k.star,
      beta.clu.ward     = beta.clu.ward,
      beta.clu.method2  = beta.clu.method2,
      return.list       = TRUE
    )
    clustz <- cc_get_cluster(cluster.list$beta.list, n)
  } else {
    cluster.list <- apply(k.star, 1, cc_max_proportion,
      beta.clu.ward     = beta.clu.ward,
      beta.clu.method2  = beta.clu.method2,
      return.list       = TRUE
    )
    clustz <- sapply(cluster.list,
      function(lasim) cc_get_cluster(lasim$beta.list, n)
    )
  }

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
       "Silhouette"      = max(unlist(Sil)),
       "n.total"         = n,
       "n.clustered"     = n.clustered
 )
}