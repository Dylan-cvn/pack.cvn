#' Trier un vecteur (équivalent de `sort`)
#'
#' @description
#' Trie un vecteur comme `base::sort()`. Tu peux ensuite faire `ordre(x)[k]`
#' pour obtenir la k-ième statistique d'ordre.
#'
#' @param x Vecteur à trier.
#' @param decreasing Logique : tri décroissant ? (FALSE par défaut)
#' @param na.last NA/TRUE/FALSE : où placer les NA.
#' @param partial Entier(s) pour tri partiel (comme dans `sort()`).
#' @param ... Autres arguments passés à `base::sort()`.
#' @return Le vecteur trié.
#'
#' @examples
#' x <- c(5, 2, 9, 1)
#' ordre(x)      # 1 2 5 9
#' ordre(x)[3]   # 5
#'
#' @export
ordre <- function(x, decreasing = FALSE, na.last = NA, partial = NULL, ...) {
  base::sort(x, decreasing = decreasing, na.last = na.last, partial = partial, ...)
}

#' Alias de `ordre()`
#'
#' @rdname ordre
#' @export
Ordre <- ordre
