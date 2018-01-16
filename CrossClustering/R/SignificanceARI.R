#' SignificanceARI
#'
#' This function performs a test for testing the null hypothesis of random
#' agreement (i.e., adjusted Rand Index equal to 0) between two partitions.
#'
#' @param ground_truth The actual membership of elements in clusters
#' @param partition The partition coming from a clustering algorithm
#'
#' @return
#' \item{Rand}{the Rand Index}
#' \item{ExpectedRand}{expected value of  Rand Index}
#' \item{AdjustedRand}{Adjusted Rand Index}
#' \item{varARI}{variance of Rand Index}
#' \item{NARI}{NARI}
#' \item{p-value}{the p-value of the test}
#'
#'
#' @examples
#' ### Two moons data
#' data(twomoons)
#' d <- dist(twomoons[,1:2], method = "euclidean")
#' CCmoons <- CrossClusteringSingle(d,k.w.max=9,k.s.max=10)
#' CCmoons_clusters <- sapply(1:dim(twomoons)[1], geneinlista,
#' CCmoons$Cluster.list)
#' CCmoons_clusters[CCmoons_clusters == "integer(0)"] <- 0
#' CCmoons_clusters <- unlist(CCmoons_clusters) + 1
#'
#' SignificanceARI(twomoons[,3], CCmoons_clusters)
#'
#' @author
#' Paola Tellaroli, <paola [dot] tellaroli [at] unipd [dot] it>;
#' Philippe Courcoux, <philippe [dot] courcoux [at] oniris-nantes [dot] fr>
#'
#' @references
#' E.M. Qannari, P. Courcoux and Faye P. (2014) Significance test of the
#' adjusted Rand index. Application to the free sorting task, Food Quality and
#' Preference, (32)93-97
#'
#' L. Hubert and P. Arabie (1985) Comparing partitions, Journal of
#' Classification, 2, 193-218.


SignificanceARI <- function(ground_truth, partition) {

  nitem <- length(ground_truth)

  Table <- table(ground_truth, partition)
  nt    <- rowSums(Table)
  pt    <- colSums(Table)

  t <- nitem * (nitem - 1) / 2
  P <- sum(nt^2) - nitem
  Q <- sum(pt^2) - nitem

  Pprime <- sum(nt * (nt - 1) * (nt - 2))
  Qprime <- sum(pt * (pt - 1) * (pt - 2))
  Q - sum(pt^2) - nitem

  a <- sum(nt * (nt - 1) / 2)
  b <- sum(pt * (pt - 1) / 2)

  n <- sum(Table * (Table - 1) / 2)

  varB   <- (1 / t) +
            (4 * Pprime * Qprime / (nitem * (nitem - 1) * (nitem-2) * P * Q)) +
            (
              (P - 2 - 4 * (Pprime / P)) *
              (Q - 2 - 4 * (Qprime / Q)) /
              (nitem * (nitem - 1) * (nitem - 2) * (nitem - 3))
            ) -
            (P * Q / (nitem^2 * (nitem - 1)^2))

  varRI  <- 4 * P * Q * varB / (nitem^2 * (nitem - 1)^2)

  expR   <- 1 - ((P + Q) / (2 * t)) + ((P * Q) / (2 * t^2))

  varARI <- varRI * (1 / ((1 - expR)^2))
  T      <- sum(Table^2) - nitem

  R      <- (T - (P / 2) - (Q / 2) + t) / t

  RandAdjusted <- (R - expR) / (1 - expR)

  NARI      <- RandAdjusted / sqrt(varARI)
  p.valNARI <- 1 - pnorm(q = NARI)

  list(
    Rand         = R,
    ExpectedRand = expR,
    AdjustedRand = RandAdjusted,
    varARI       = varARI,
    NARI         = NARI,
    p.value      = p.valNARI
  )
}

