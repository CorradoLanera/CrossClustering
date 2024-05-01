#' Reverse the process of create a contingency table
#'
#' @param x a contingency table
#'
#' @return a list of 2 vector corresponding to the unrolled table
#' @export
#'
#' @examples
#' clust_1 <- iris[, 1:4] |>
#'   dist() |>
#'   hclust() |>
#'   cutree(k = 3)
#' clust_2 <- iris[, 1:4] |>
#'   dist() |>
#'   hclust() |>
#'   cutree(k = 4)
#' cont_table <- table(clust_1, clust_2)
#'
#' reverse_table(cont_table)
reverse_table <- function(x) {
  # input check
  checkmate::qassert(x, "X+")

  stats::setNames(
    list(
      rep(seq_len(nrow(x)), margin.table(x, 1L)),
      rep(seq_len(ncol(x)), margin.table(x, 2L))
    ),
    names(dimnames(x))
  )
}
