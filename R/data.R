#' A toy dataset for illustrating the chain effect.
#'
#' @format A data frame with 28 rows and 2 variables:
#'
#' \describe{
#'    \item{`X`}{num} x coordinates
#'      0 is negative.
#'    \item{`Y`}{num} y coordinates.
#'    }
#'
"chain_effect"

#' A famous shape data set containing two clusters with two moons shapes and
#' outliers
#'
#' @format A data frame with 52 rows and 3 variables:
#'
#' \describe{
#'    \item{`x`}{num} x coordinates
#'    \item{`y`}{num} y coordinates.
#'    \item{`clusters`}{integer} cluster membership (outliers
#'          classified as 3rd cluster).
#' }
#'
"twomoons"


#' A famous shape data set containing two clusters with two worms shapes and
#' outliers
#'
#' @format A data frame with 87 rows and 3 variables:
#'
#' \describe{
#'    \item{`x`}{num} x coordinates
#'    \item{`y`}{num} y coordinates.
#'    \item{`cluster`}{integer} cluster membership (outliers classified
#'          as 3rd cluster).
#' }
#'
"worms"

#' A toy example matrix
#'
#' @format A matrix of 10 row and 7 columns
#'
"toy"



#' RNA-Seq dataset example
#'
#' `nb_data` contains a subset of a bigger normalized negative binomial
#' simulated dataset.
#'
#' This dataset is part of a larger simulated and normalized dataset with 2
#' experimental groups, 6 time-points and 3 replicates. Simulation has been done
#' by using a negative binomial distribution. The first 20 genes are simulated
#' with changes among time.
#'
#' @format A data frame with 100 observations on 36 numeric variables.
#'
#' @source Data included in the bioconductor package `maSigPro`.
#'   <https://doi.org/doi:10.18129/B9.bioc.maSigPro>
"nb_data"
