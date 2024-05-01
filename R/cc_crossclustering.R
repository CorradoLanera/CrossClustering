#' A partial clustering algorithm with automatic estimation of the
#' number of clusters and identification of outliers
#'
#' This function performs the CrossClustering algorithm. This method
#' combines the Ward's minimum variance and Complete-linkage (default,
#' useful for finding spherical clusters) or Single-linkage (useful for
#' finding elongated clusters) algorithms, providing automatic
#' estimation of a suitable number of clusters and identification of
#' outlier elements.
#'
#' @param dist A dissimilarity structure as produced by the function
#'   `dist`
#' @param k_w_min (int) Minimum number of clusters for the Ward's
#'   minimum variance method. By default is set equal 2
#' @param k_w_max (int) Maximum number of clusters for the Ward's
#'   minimum variance method (see details)
#' @param k2_max (int) Maximum number of clusters for the
#'   Complete/Single-linkage method. It can not be equal or greater than
#'   the number of elements to cluster (see details)
#' @param out (lgl) If `TRUE` (default) outliers must be searched
#'   (see details)
#' @param method (chr) "complete" (default) or "single". CrossClustering
#'   combines Ward's algorithm with Complete-linkage if method is set to
#'   "complete", otherwise (if method is set to 'single') Single-linkage
#'   will be used.
#' @return A list of objects describing characteristics of the
#'   partitioning as follows:
#'     \item{Optimal_cluster}{number of clusters}
#'     \item{cluster_list_elements}{a list of clusters; each element of this
#'       lists contains the indices of the elements belonging to the
#'       cluster}
#'     \item{Silhouette}{the average silhouette width over all the clusters}
#'     \item{n_total}{total number of input elements}
#'     \item{n_clustered}{number of input elements that have actually
#'       been clustered}
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
#' toy_dist <- t(toy) |>
#'   dist(method = "euclidean")
#'
#' ### Run CrossClustering
#' cc_crossclustering(
#'   toy_dist,
#'   k_w_min = 2,
#'   k_w_max = 5,
#'   k2_max = 6,
#'   out = TRUE
#' )
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
#' for (i in seq_len(nrow(t))) {
#'   t_mat <- rbind(
#'     t_mat,
#'     matrix(rep(t[i, ], sg[i]), nrow = sg[i], byrow = TRUE)
#'   )
#' }
#'
#' data_15 <- matrix(NA, nrow = 2000, ncol = 5)
#' data_15[1:1850, ] <- matrix(
#'   abs(rnorm(sum(sg) * 5, sd = 1.5)),
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
#' cc_crossclustering(
#'   dist(data_15),
#'   k_w_min = 2,
#'   k_w_max = 19,
#'   k2_max = 20,
#'   out = TRUE
#' )
#'
#'
#' #### Correlation-based distance is often used in gene expression time-series
#' ### data analysis. Here there is an example, using the "complete" method.
#'
#' data(nb_data)
#' nb_dist <- as.dist(1 - abs(cor(t(nb_data))))
#' cc_crossclustering(dist = nb_dist, k_w_max = 20, k2_max = 19)
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
#' moons_dist <- twomoons[, 1:2] |>
#'   dist(method = "euclidean")
#'
#' cc_moons <- cc_crossclustering(
#'   moons_dist,
#'   k_w_max = 9,
#'   k2_max = 10,
#'   method = 'single'
#' )
#'
#' moons_col <- cc_get_cluster(cc_moons)
#' plot(
#'   twomoons[, 1:2],
#'   col = moons_col,
#'   pch      = 19,
#'   xlab     = "",
#'   ylab     = "",
#'   main     = "CrossClustering-Single"
#' )
#'
#' ### Worms data
#' data(worms)
#'
#' worms_dist <- worms[, 1:2] |>
#'   dist(method = "euclidean")
#'
#' cc_worms <- cc_crossclustering(
#'   worms_dist,
#'   k_w_max = 9,
#'   k2_max  = 10,
#'   method  = "single"
#' )
#'
#' worms_col <-  cc_get_cluster(cc_worms)
#'
#' plot(
#'   worms[, 1:2],
#'   col = worms_col,
#'   pch = 19,
#'   xlab = "",
#'   ylab = "",
#'   main = "CrossClustering-Single"
#' )
#'
#'
#' ### CrossClustering-Single is not affected to chain-effect problem
#'
#' data(chain_effect)
#'
#' chain_dist <- chain_effect |>
#'   dist(method = "euclidean")

#' cc_chain <- cc_crossclustering(
#'   chain_dist,
#'   k_w_max = 9,
#'   k2_max = 10,
#'   method = "single"
#' )
#'
#' chain_col <- cc_get_cluster(cc_chain)
#'
#' plot(
#'   chain_effect,
#'   col = chain_col,
#'   pch = 19,
#'   xlab = "",
#'   ylab = "",
#'   main = "CrossClustering-Single"
#' )
#'
#' @author
#' Paola Tellaroli, <paola `dot` tellaroli `at` unipd `dot` it>;;
#' Marco Bazzi, <bazzi `at` stat `dot` unipd `dot` it>;
#' Michele Donato, <mdonato `at` stanford `dot` edu>
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


cc_crossclustering <- function(dist,
  k_w_min = 2,
  k_w_max = attr(dist, "Size") - 2,
  k2_max  = k_w_max + 1,
  out     = TRUE,
  method  = c("complete", "single")
) {
  method <- match.arg(method)

  # imput check
  checkmate::assert_class(dist, "dist")
  checkmate::qassert(k_w_min, "X1(0,)")
  checkmate::qassert(k_w_min, "X1(0,)")
  checkmate::assert_integerish(k_w_max, lower = k_w_min)
  checkmate::qassert(k2_max, "X1(0,)")
  checkmate::assert_integerish(k2_max, lower = k_w_min)

  checkmate::qassert(out, "B1")
  checkmate::qassert(method, "S1")

  n <- attr(dist, "Size")

  cluster_ward  <- hclust(dist, method = "ward.D")
  cluster_other <- hclust(dist, method = method)

  grid <- as.matrix(expand.grid(k_w_min:k_w_max, k_w_min:k2_max))

  if (out) {
    grid <- grid[grid[, 2] >  grid[, 1], ]
  } else {
    grid <- grid[grid[, 2] >= grid[, 1], ]
  }

  n_clu <- vector("list", length = nrow(grid))

  for (i in seq_len(nrow(grid))) {
    n_clu[[i]] <- consensus_cluster(grid[i, ],
      cluster_ward  = cluster_ward,
      cluster_other = cluster_other
    )[["max_consensus"]]
  }

  grid <- cbind(grid, unlist(n_clu))
  colnames(grid) <- c("Ward", method, "N. classified")

  grid_star <- which(grid == max(grid[, 3]), arr.ind = TRUE)[, 1]
  k_star    <- rbind(grid[grid_star, 1:2])

  if (is.null(dim(k_star))) {
    cluster_list <- consensus_cluster(k_star,
      cluster_ward  = cluster_ward,
      cluster_other = cluster_other
    )
    clustz <- cc_get_cluster(cluster_list$elements, n)
  } else {
    cluster_list <- apply(k_star, 1, consensus_cluster,
      cluster_ward  = cluster_ward,
      cluster_other = cluster_other
    )
    clustz <- sapply(cluster_list,
      function(lasim) cc_get_cluster(lasim$elements, n)
    )
  }

  if (is.null(dim(clustz))) {
    clustz <- matrix(clustz, ncol = 1)
  }

  sil <- vector("double", length = ncol(clustz))

  for (c in seq_len(ncol(clustz))) {
    sil[[c]] <- mean(
      cluster::silhouette(as.numeric(clustz[, c]), dist = dist)[, 3]
    )
  }

  cluster_list_elements <- cluster_list[[which.max(sil)]]$elements
  cluster_list_elements <- stats::setNames(cluster_list_elements,
    paste("cluster", seq_along(cluster_list_elements), sep = "_")
  )

  n_clustered  <- length(unlist(cluster_list_elements))



  structure(cluster_list_elements,
    optimal_cluster = length(cluster_list[[which.max(sil)]]$elements),
    silhouette      = max(unlist(sil)),
    n_total         = n,
    n_clustered     = n_clustered,
    input           = list(
      dist    = dist,
      k_w_min = k_w_min,
      k_w_max = k_w_max,
      k2_max  = k2_max,
      out     = out,
      method  = method
    ),
    class = "crossclustering"
  )
}


#' print method for crossclustering class
#'
#' @inheritParams base::print
#' @describeIn cc_crossclustering
#'
#' @export
print.crossclustering <- function(x, ...) {
  cat(paste0("\n",
    "    CrossClustering with method ",
    crayon::green(attr(x, "input")[["method"]]), ".\n",
    "\n"
  ))
  cat(paste0("Parameter used:\n",
    "  - Interval for the number of cluster of Ward's algorithm: [",
    crayon::green(attr(x, "input")[["k_w_min"]]), ", ",
    crayon::green(attr(x, "input")[["k_w_max"]]), "].\n",
    "  - Interval for the number of cluster of the ",
    crayon::green(attr(x, "input")[["method"]]),
    " algorithm: [", crayon::green(attr(x, "input")[["k_w_min"]]), ", ",
    crayon::green(attr(x, "input")[["k2_max"]]), "].\n"
  ))
  cat(paste0("  - Outliers ", crayon::green("are "),
    if (!attr(x, "input")[["out"]]) crayon::red("NOT "), "considered.",
    "\n\n"
  ))
  cat(paste0("Number of clusters found: ",
    crayon::blue(attr(x, "optimal_cluster")), ".\n",
    "Leading to an avarage silhouette width of: ",
    crayon::blue(round(attr(x, "silhouette"), 4)), ".\n",
    "\n"
  ))
  cat(paste0("A total of ",
    crayon::blue(attr(x, "n_clustered")), " elements clustered out of ",
    crayon::blue(attr(x, "n_total")), " elements considered."
  ))
}
