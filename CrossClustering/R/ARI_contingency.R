#' Computes the adjusted Rand index comparing two classifications from a contingency table.
#'
#' @param mat The contingency table
#'
#' @return
#' The adjusted Rand index.
#'
#' @examples
#' mat <- matrix(c(4,5,3,3,8,4),ncol=3,byrow = TRUE)
#' ARI_contingency(mat)
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#'
#' @references
#' L. Hubert and P. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.

ARI_contingency <- function(mat){
  n_pox <- choose(sum(colSums(mat)), 2)
  p = 0
  num1 <- numeric(nrow(mat)*ncol(mat))
  num2 <- numeric(ncol(mat))
  num3 <- numeric(nrow(mat))
  for (i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      p <- p + 1
      num1[[p]] <- choose(mat[i,j],2)
      num2[[j]] <- choose(colSums(mat)[j],2)
      num3[[i]] <- choose(rowSums(mat)[i],2)
      exp <- (sum(num2)*sum(num3)/n_pox)
      num <- sum(num1) - exp
      den <- 0.5*(sum(num2)+sum(num3))-exp
    }
  }
  (ARI <- num/den)
}


