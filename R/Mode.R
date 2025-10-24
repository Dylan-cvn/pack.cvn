#' Mode statistique
#'
#' Renvoie la/les valeur(s) la/les plus fréquente(s) d'un vecteur.
#'
#' @param x Vecteur (numérique, factor ou character).
#' @param na_rm TRUE pour ignorer les NA.
#' @param ties Que faire en cas d'égalité : "all" (toutes), "first" (la 1re),
#'   ou "random" (une au hasard).
#' @return Si \code{ties = "all"} : un vecteur des modes ; sinon une seule valeur.
#' @examples
#' Mode(c(1,2,2,3))               # 2
#' Mode(c(1,2,2,3,3), ties="all") # 2 3
#' @export
Mode <- function(x, na_rm = TRUE, ties = c("all","first","random")) {
  ties <- match.arg(ties)
  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA)

  tb <- table(x, useNA = "no")
  m  <- max(tb)
  modes <- names(tb)[tb == m]

  # remet "120" -> 120 si possible
  modes <- type.convert(modes, as.is = TRUE)

  switch(ties,
    all    = modes,
    first  = modes[[1]],
    random = sample(modes, 1)
  )
}
