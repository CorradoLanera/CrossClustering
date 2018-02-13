# CrossClustering 3.1.42

* Renamed `which_cluster()` to `cc_get_cluster()`
* Renamed `SignificanceARI()` to `cc_test_ari()`
* Renamed `PermSignificanceARI()` to `cc_test_ari_permutation()`
* Renamed `max_proportion_function()` to `cc_max_proportion()`
* Renamed `CrossClustering()` to `cc_crossclustering()`
* Renamed `ARI_contingency()` to `cc_ari_contingency()`

# CrossClustering 3.1.35

* Added test for `ARI_contingency()` as requestedi in issue-#7

# CrossClustering 3.1.34

* Added examples to main functions
* Adopted a verb-like style for the function names
* Added `data/`, `data-raw/` and `R/data.R` to include example data into the
  package.
* Adopted snake_case for funciton and variable names
* Added functions: `ARI_contingency()`, `PermSignificanceARI()` and
  `SignificanceARI()`.
* Added support for _complete_ and _single_ method to `CrossClustering()`
* Removed all the calls to `:` in favor of `seq_*()`
* Removed all the calls to `require()` or `library()`
* Removed all the calls to `sapply()`
* Substituted `geneinlista()` with `which_cluster()`
* Reshaped directory tree
* Added `dependencies.R` to track imported dependencies
* Added `utils-pip.R` to support pipe operator
* Added `utils.R` for utility functions
* Restyled all the code
* Adde tests modulus for all the funcitons
* Added support for Travis, Appveyor and Codecov CI 
* Added a `NEWS.md` file to track changes to the package.
