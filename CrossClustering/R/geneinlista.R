#' geneinlista
#'
#' @param mygene
#' @param lista
#'
#' @return
#'
#' @examples
geneinlista <- function(mygene, lista){
  which(sapply(lista, function(gene, elem) gene %in% elem, gene = mygene))
}
