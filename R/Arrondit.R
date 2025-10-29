#' Arrondit des valeurs numériques
#' Enveloppe simple autour de `base::round()`.
#'
#' @param x Un vecteur (ou matrice) numérique.
#' @param digits Entier : nombre de décimales (peut être négatif).
#'
#' @return Un objet du même type que `x`, avec les valeurs arrondies.
#'
#' @details
#' Utilise l'arrondi "vers le pair" (banker's rounding) de R.
#' Pour éviter tout conflit, on appelle explicitement `base::round()`.
#'
#' @examples
#' Arrondit(3.14159, 2)              # 3.14
#' Arrondit(c(2.5, 3.5))             # 2, 4
#' Arrondit(1234, -2)                # 1200
#'
#' @export
Arrondit <- function(x, digits = 0) {
  if (!is.numeric(x)) {
    stop("`x` doit être numérique.", call. = FALSE)
  }
  if (length(digits) != 1 || !is.finite(digits)) {
    stop("`digits` doit être un nombre fini de longueur 1.", call. = FALSE)
  }
  digits <- as.integer(digits)
  base::round(x, digits = digits)
}
