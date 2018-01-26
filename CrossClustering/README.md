<!-- README.md is generated from README.Rmd. Please edit that file -->
CrossClustering
===============

CrossClustering is a partial clustering algorithm that combines the Ward's minimum variance and Complete Linkage algorithms, providing automatic estimation of a suitable number of clusters and identification of outlier elements.

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library(CrossClustering)

#### method = "complete"
### Generate simulated data
toy <- matrix(NA, nrow = 10, ncol = 7)
colnames(toy) <- paste("Sample", 1:ncol(toy), sep = "")
rownames(toy) <- paste("Gene"  , 1:nrow(toy), sep = "")
set.seed(123)

toy[, 1:2] <- rnorm(n = nrow(toy) * 2, mean = 10, sd  = 0.1)
toy[, 3:4] <- rnorm(n = nrow(toy) * 2, mean = 20, sd  = 0.1)
toy[, 5:6] <- rnorm(n = nrow(toy) * 2, mean = 5 , sd  = 0.1)
toy[,   7] <- runif(n = nrow(toy)    , min  = 0 , max = 1  )

### toy is transposed as we want to cluster samples (columns of the original
### matrix)
d <- dist(t(toy), method = "euclidean")

### Run CrossClustering
CrossClustering(d, k.w.min = 2, k.w.max = 5, k2.max = 6,
out = TRUE)
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

Install
-------

CrossClustering package is on CRAN, use the standard method to install it. `install.packages('CrossClustering')`

Feature request
---------------

If you need some more features, please write to Paola Tellaroli <paola.tellaroli@unipd.it>

Bug reports
-----------

If you encounter a bug, please file a [reprex](https://github.com/tidyverse/reprex) (minimal reproducible example) to Paola Tellaroli K<paola.tellaroli@unipd.it>
