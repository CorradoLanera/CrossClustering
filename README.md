<!-- README.md is generated from README.Rmd. Please edit that file -->
CrossClustering
===============

[![Travis Build Status](https://travis-ci.org/CorradoLanera/CrossClustering_svg?branch=develop)](https://travis-ci.org/CorradoLanera/CrossClustering) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/CorradoLanera/CrossClustering?branch=develop&svg=true)](https://ci.appveyor.com/project/CorradoLanera/CrossClustering) <!-- [![CRAN Status Badge](http://www.r-pkg.org/badges/version/CrossClustering)](http://cran.R-project.org/package=CrossClustering) --> [![Coverage Status](https://codecov.io/gh/CorradoLanera/CrossClustering/branch/develop/graph/badge_svg)](https://codecov.io/gh/CorradoLanera/CrossClustering?branch=develop)

CrossClustering is a partial clustering algorithm that combines the Ward's minimum variance and Complete Linkage algorithms, providing automatic estimation of a suitable number of clusters and identification of outlier elements.

Example
-------

This is a basic example which shows you how to the main function, i.e. `cc_crossclustering()` works:

``` r
## basic example code
library(CrossClustering)

#### method = "complete"
data(toy)

### toy is transposed as we want to cluster samples (columns of the original
### matrix)
d <- dist(t(toy), method = "euclidean")

### Run CrossClustering
toyres <- cc_crossclustering(d, k_w_min = 2, k_w_max = 5, k2_max = 6,
out = TRUE)
toyres
#> $Optimal_cluster
#> [1] 3
#> 
#> $Cluster_list
#> $Cluster_list[[1]]
#> [1] 1 2
#> 
#> $Cluster_list[[2]]
#> [1] 3 4
#> 
#> $Cluster_list[[3]]
#> [1] 5 6
#> 
#> 
#> $Silhouette
#> [1] 0.8405204
#> 
#> $n_total
#> [1] 7
#> 
#> $n_clustered
#> [1] 6
```

Another useful function worth to mention is `cc_test_ari_permutation`:

``` r
CC_clusters <- cc_get_cluster(toyres$Cluster_list, toyres$n_total)
cc_test_ari_permutation(
  ground_truth = c(1, 1, 2, 2, 3, 3, 4),
  partition    = CC_clusters
)
#>   Stat p-value
#> 1    1   0.006
```

Install
-------

CrossClustering package is on CRAN, use the standard method to install it. `install_packages('CrossClustering')`

Feature request
---------------

If you need some more features, please write to Paola Tellaroli <paola.tellaroli@unipd.it>

Bug reports
-----------

If you encounter a bug, please file a [reprex](https://github.com/tidyverse/reprex) (minimal reproducible example) to Paola Tellaroli K<paola.tellaroli@unipd.it>
