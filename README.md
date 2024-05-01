<!-- README.md is generated from README.Rmd. Please edit that file -->

# CrossClustering

[![Travis Build
Status](https://travis-ci.org/CorradoLanera/CrossClustering.svg?branch=develop)](https://travis-ci.org/CorradoLanera/CrossClustering)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/CorradoLanera/CrossClustering?branch=develop&svg=true)](https://ci.appveyor.com/project/CorradoLanera/CrossClustering)
[![CRAN Status
Badge](http://www.r-pkg.org/badges/version/CrossClustering)](https://CRAN.R-project.org/package=CrossClustering)
[![Coverage
Status](https://codecov.io/gh/CorradoLanera/CrossClustering/branch/develop/graph/badge.svg)](https://codecov.io/gh/CorradoLanera/CrossClustering?branch=develop)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

CrossClustering is a partial clustering algorithm that combines the
Ward’s minimum variance and Complete Linkage algorithms, providing
automatic estimation of a suitable number of clusters and identification
of outlier elements.

## Example

This is a basic example which shows you how to the main function, i.e. 
`cc_crossclustering()` works:

``` r
## basic example code
library(CrossClustering)

#### method = "complete"
data(toy)

### toy is transposed as we want to cluster samples (columns of the original
### matrix)
d <- dist(t(toy), method = "euclidean")

### Run CrossClustering
toyres <- cc_crossclustering(
  d, k_w_min = 2, k_w_max = 5, k2_max = 6, out = TRUE
)
toyres
#> 
#>     CrossClustering with method complete.
#> 
#> Parameter used:
#>   - Interval for the number of cluster of Ward's algorithm: [2, 5].
#>   - Interval for the number of cluster of the complete algorithm: [2, 6].
#>   - Outliers are considered.
#> 
#> Number of clusters found: 3.
#> Leading to an avarage silhouette width of: 0.8405.
#> 
#> A total of 6 elements clustered out of 7 elements considered.
```

Another useful function worth to mention is `ari`:

``` r
clusters <- iris[-5] |>
 dist() |>
 hclust(method = 'ward.D') |>
 cutree(k = 3)

ground_truth <- iris[[5]] |>
  as.numeric()

table(ground_truth, clusters) |> 
  ari()
#>     Adjusted Rand Index (alpha = 0.05)
#> 
#> ARI                  = 0.76 (moderate recovery)
#> Confidence interval  = [0.74, 0.78]
#> 
#> p-values:
#>   * Qannari test     = < 0.001
#>   * Permutation test =   0.001
```

## Install

### CRAN version

CrossClustering package is on CRAN, use the standard method to install
it. `install_packages('CrossClustering')`

### develop version

To install the develop branch of CrossClastering package, use:

``` r
# install.packages(devtools)
devtools::install_github('CorradoLanera/CrossClustering', ref = 'develop')
```

## Bug reports

If you encounter a bug, please file a
[reprex](https://github.com/tidyverse/reprex) (minimal reproducible
example) to <https://github.com/CorradoLanera/CrossClustering/issues>

## References

**Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2016).
Cross-Clustering: A Partial Clustering Algorithm with Automatic
Estimation of the Number of Clusters. PLoS ONE 11(3): e0152333.
<https://doi.org/10.1371/journal.pone.0152333>**

**Tellaroli P, Bazzi M., Donato M., Brazzale A. R., Draghici S. (2017).
E1829: Cross-Clustering: A Partial Clustering Algorithm with Automatic
Estimation of the Number of Clusters. CMStatistics 2017, London 16-18
December, Book of Abstracts (ISBN 978-9963-2227-4-2)**
