#' Prune tail made of zeros
#'
#' Given a diagonal matrix which is supposed to have no non-zero entry in
#' the diagonal after the first one (if any) the  function returns the
#' diagonal (sub-)matrix without the columns and the row corresponding to
#' the zero-entries in the diagonal (if any).
#'
#' @param diag_mat a diagonal matrix which must satisfy the following
#'   property: in the diagonal, every element after a zero is a zero.
#'
#' @return a diagonal matrix without zeros in the diagonal, composed by the
#'   first rows and columns of the original matrix with non zeros in the
#'   diagonal (which are also the only ones)
#' @export
#'
#' @examples
#' diag_mat <- diag(c(1, 2, 3, 0, 0, 0, 0))
#' prune_zero_tail(diag_mat)
prune_zero_tail <- function(diag_mat) {
  # input check
  diag_check <- diag_mat
  diag(diag_check) <- 0
  checkmate::qassert(diag_check, "N+[0,0]")
  rm(diag_check)

  # detect the index of the first zero in the diagonal
  # note: detect_index run the computation only until the first match
  first_zero <- diag(diag_mat) |>
    purrr::detect_index(is_zero)

  # no zero-entry
  if (first_zero == 0) return(diag_mat)

  # some zero-entry
  max_pos <- first_zero - 1

  if (max_pos != sum(diag_mat != 0)) stop(
    "diag_mat cannot have non-zeros after the first zero in the diagonal"
  )

  taken_id <- seq_len(first_zero - 1)
  diag_mat[taken_id, taken_id]
}


#' Check for zero
#'
#' Check if a given, single, number is 0 or not
#'
#' @param num a numerical vector of length one
#'
#' @return a boolean, TRUE if num is 0
#' @export
#' @examples
#' is_zero(1)
#' is_zero(0)
is_zero <- function(num) {
  # input check
  checkmate::qassert(num, "N1")

  # return the results
  num == 0
}
