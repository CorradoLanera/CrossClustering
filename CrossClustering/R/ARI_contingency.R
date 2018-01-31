#' Computes the adjusted Rand index and the confidence interval, comparing two
#' classifications from a contingency table.
#'
#' @param mat The contingency table
#' @param alpha The significance level
#'
#' @return
#' A vector of objects:
#' \item{AdjustedRandIndex}{The adjusted Rand Index}
#' \item{CI}{The confidence interval}
#' @examples
#' mat <- matrix(c(4,5,3,3,8,4),ncol=3,byrow = TRUE)
#' ARI_contingency(mat, alpha = 0.05)
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;;
#'
#' @references
#' L. Hubert and P. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.
#' D. Steinley, M. J. Brusco, L. Hubert (2016) The Variance of the Adjusted
#' Rand Index, Psychological Methods, 21(2), 261-272

ARI_contingency <- function(mat, alpha){
  n_pox <- choose(sum(mat), 2)
  N <- sum(mat)
  p = 0
  num1 <- cell <- numeric(nrow(mat)*ncol(mat))
  num2 <- numeric(ncol(mat))
  num3 <- numeric(nrow(mat))
  for (i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      p <- p + 1
      num1[[p]] <- choose(mat[i,j],2)
      num2[[j]] <- choose(colSums(mat)[j],2)
      num3[[i]] <- choose(rowSums(mat)[i],2)
      cell[[p]] <- mat[i,j]
    }
  }
  a <- (sum(cell^2)-N)/2
  b <- (sum(rowSums(mat)^2)-(sum(cell^2)))/2
  c <- (sum(colSums(mat)^2)-(sum(cell^2)))/2
  d <- (sum(cell^2)+N^2-sum(rowSums(mat)^2)-sum(colSums(mat)^2))/2

  exp <- (sum(num2)*sum(num3)/n_pox)
  num <- sum(num1) - exp
  den <- 0.5*(sum(num2)+sum(num3))-exp

  e <- 2*sum(rowSums(mat)^2)-(N+1)*N
  f <- 2*sum(colSums(mat)^2)-(N+1)*N
  g <- 4*sum(rowSums(mat)^3)-4*(N+1)*sum(rowSums(mat)^2)+(N+1)^2*N
  h <- N*(N-1)
  i <- 4*sum(colSums(mat)^3)-4*(N+1)*sum(colSums(mat)^2)+(N+1)^2*N

  var_ad <- 1/16*(2*N*(N-1)-
                    ((e*f)/(N*(N-1)))^2+
                    (4*(g-h)*(i-h))/(N*(N-1)*(N-2)))+
    1/16*(((e^2-4*g+2*h)*(f^2-4*i+2*h))/(N*(N-1)*(N-2)*(N-3)))

  var_ARI <- (n_pox^2*var_ad)/((n_pox^2-((a+b)*(a+c)+(b+d)*(c+d)))^2)

  ARI <- num/den
  LL <- ARI-qnorm(p=(1-alpha/2))*sqrt(var_ARI)
  UL <- ARI+qnorm(p=(1-alpha/2))*sqrt(var_ARI)

  c("AdjustedRandIndex" = round(ARI, digits = 2),
        "CI"   = paste(round(LL, digits = 2),
                       round(UL, digits = 2),
                       sep = "; "))

}

