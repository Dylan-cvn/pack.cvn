#' Ordre (équivalent de sort)
#'
#' Trie un vecteur comme `base::sort`.
#' Permet `Ordre(x)[k]` pour obtenir la k-ième statistique d'ordre.
#'
#' @param x Vecteur à trier.
#' @param decreasing Logique; tri décroissant ? (FALSE par défaut)
#' @param na.last NA/TRUE/FALSE : où placer les NA.
#' @param partial Entier(s) pour tri partiel (comme dans `sort()`).
#' @param ... Autres arguments passés à `sort()`.
#' @return Le vecteur trié.
#' @examples
#' x <- c(5, 2, 9, 1)
#' Ordre(x)        # 1 2 5 9
#' Ordre(x)[3]     # 5
#' @export
Ordre <- function(x, decreasing = FALSE, na.last = NA, partial = NULL, ...) {
  sort(x, decreasing = decreasing, na.last = na.last, partial = partial, ...)
}

