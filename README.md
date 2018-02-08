<!-- README.md is generated from README.Rmd. Please edit that file -->
CrossClustering
===============

[![Build Status](https://travis-ci.com/CorradoLanera/CrossClustering.svg?branch=develop)](https://travis-ci.com/CorradoLanera/CrossClustering) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/CorradoLanera/CrossClustering?branch=develop&svg=true)](https://ci.appveyor.com/project/CorradoLanera/CrossClustering) <!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/CrossClustering)](http://cran.r-project.org/package=CrossClustering) --> [![Coverage Status](https://codecov.io/gh/CorradoLanera/CrossClustering/branch/develop/graph/badge.svg)](https://codecov.io/gh/CorradoLanera/CrossClustering?branch=develop)

CrossClustering is a partial clustering algorithm that combines the Ward's minimum variance and Complete Linkage algorithms, providing automatic estimation of a suitable number of clusters and identification of outlier elements.

Example
-------

This is a basic example which shows you how to the main function, i.e. `CrossClustering()` works:

``` r
## basic example code
library(CrossClustering)

#### method = "complete"
data(toy)

### toy is transposed as we want to cluster samples (columns of the original
### matrix)
d <- dist(t(toy), method = "euclidean")

### Run CrossClustering
toyres <- CrossClustering(d, k.w.min = 2, k.w.max = 5, k2.max = 6,
out = TRUE)
toyres
#> $Optimal.cluster
#> [1] 3
#> 
#> $Cluster.list
#> $Cluster.list[[1]]
#> [1] 1 2
#> 
#> $Cluster.list[[2]]
#> [1] 3 4
#> 
#> $Cluster.list[[3]]
#> [1] 5 6
#> 
#> 
#> $Silhouette
#> [1] 0.8405204
#> 
#> $n.total
#> [1] 7
#> 
#> $n.clustered
#> [1] 6
```

Another useful function worth to mention is `PermSignificanceARI`:

``` r
CC_clusters <- which_cluster(toyres$Cluster.list, toyres$n.total)
PermSignificanceARI(
  ground_truth = c(1, 1, 2, 2, 3, 3, 4),
  partition    = CC_clusters
)
#>   Stat p-value
#> 1    1   0.007
```

Install
-------

CrossClustering package is on CRAN, use the standard method to install it. `install.packages('CrossClustering')`

Feature request
---------------

If you need some more features, please write to Paola Tellaroli <paola.tellaroli@unipd.it>

Bug reports
-----------

If you encounter a bug, please file a [reprex](https://github.com/tidyverse/reprex) (minimal reproducible example) to Paola Tellaroli K<paola.tellaroli@unipd.it>
