#' Moyenne (équivalent de mean)
#'
#' Calcule la moyenne d'un vecteur numérique.
#'
#' @param x Vecteur numérique.
#' @param ... Arguments additionnels passés à [base::mean()], par ex. `na.rm = TRUE`.
#' @return Un scalaire numérique (la moyenne).
#' @export
#' @examples
#' Moyenne(c(1, 2, 3))
#' Moyenne(c(1, NA, 3), na.rm = TRUE)
Moyenne <- function(x, ...) {
  base::mean(x, ...)
}
