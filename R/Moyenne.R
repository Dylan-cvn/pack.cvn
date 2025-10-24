#' Moyenne (équivalent de mean)
#'
#' @param x Vecteur numérique.
#' @param ... Passé à base::mean (ex. na.rm = TRUE).
#' @return Un scalaire numérique (la moyenne).
#' @export
Moyenne <- function(x, ...) {
  mean(x, ...)
}
