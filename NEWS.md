# CrossClustering 4.1.2

* exported `cc_test_ari`, `cc_test_ari_permutation`,
  `consensus_cluster`, and `is_zero`

# CrossClustering 4.1.1

* Removed `.Random.seed` after CRAN request.

# CrossClustering 4.1.0

* linted
* switch from `{magrittr}` to native pipe
* switch from `{assertive}` to `{checkmate}`

# CrossClustering 4.0.4

* Added `.github/CONTRIBUTING.md`
* Added `.github/ISSUE_TEMPLATE.md` file
* Added `CODE_OF_CONDUCT.md` file
* Added `LICENCE` file for GPL-3

# CrossClustering 4.0.3 (CRAN ACCEPTED)

* re-submission to CRAN
* fix `DESCRIPTION` issues

# CrossClustering 4.0.2

* re-submission to CRAN
* fix `DESCRIPTION` issues

# CrossClustering 4.0.1

* submit to  CRAN
* url fixed
* spellcheck
* update `.travis.yml` to fix an error in the macOS-devel build:
  `warnings_are_errors: false` for that build.

# CrossClustering 3.3.02

* Rversions 3.1 and 3.2 removed from Travis-CI
* Reformat `DESCRIPTION` file

# CrossClustering 3.3.01

* Reference updated
* Removed exported function `cc_test_ari()` and `cc_test_ari_permutation()`
  because now included in `ari()`
* Adapted code and test to the new structures and conventions
* Added dependencies for package `dplyr`
* Changed and renamed `cc_max_proportion()` in `consensus_cluster()` as 
  a constructor of object of class consensus_cluster
* Created `reverse_table()` to come back from a contingency table to the
  unrolled vector of elements (issue #13)
* Changes made in `cc_get_clust()` and `cc_crossclustering()` (issue #15)
* Added examples for correlation (issue #14)
* Changed and Renamed `cc_ari_contingency()` to `ari` as a constructor of
  objects of class ari (issue #12)
* Added package `cli` into the dependencies
* Update `DESCRIPTION`
* Add Lifecycle badge
* Add CRAN badge

# CrossClustering 3.2.14

* Added OSX on Travis-CI
* Updated README
* Minor style changes
* Changed al unnecessary use of dot (`.`) to underscore (`_`)

# CrossClustering 3.1.42

* Renamed `which_cluster()` to `cc_get_cluster()`
* Renamed `SignificanceARI()` to `cc_test_ari()`
* Renamed `PermSignificanceARI()` to `cc_test_ari_permutation()`
* Renamed `max_proportion_function()` to `cc_max_proportion()`
* Renamed `CrossClustering()` to `cc_crossclustering()`
* Renamed `ARI_contingency()` to `cc_ari_contingency()`

# CrossClustering 3.1.35

* Added test for `ARI_contingency()` as requested in issue-#7

# CrossClustering 3.1.34

* Added examples to main functions
* Adopted a verb-like style for the function names
* Added `data/`, `data-raw/` and `R/data.R` to include example data into the
  package.
* Adopted snake_case for function and variable names
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
* Added tests modulus for all the functions
* Added support for Travis, Appveyor and Codecov CI 
* Added a `NEWS.md` file to track changes to the package.
