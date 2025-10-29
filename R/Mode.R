#' Mode statistique
#'
#' Calcule la/les valeur(s) modale(s) d'un vecteur.
#'
#' @param x vecteur.
#' @param na_rm TRUE pour ignorer les NA.
#' @param ties Stratégie en cas d'ex æquo : `"all"`, `"first"`, ou `"random"`.
#' @return La/les valeur(s) modale(s) (même type que `x`).
#' @export
#' @examples
#' Mode(c(1, 2, 2, 3))                # 2
#' Mode(c(1, 1, 2, 2), ties = "all")  # c(1, 2)
#' Mode(c("a", "b", "b", "a"), ties = "first")  # "a"
Mode <- function(x, na_rm = TRUE, ties = c("all", "first", "random")) {
  ties <- match.arg(ties)

  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA)

  tb <- table(x, useNA = "no")
  modes <- names(tb)[tb == max(tb)]
  modes <- type.convert(modes, as.is = TRUE)

  switch(
    ties,
    all    = modes,
    first  = modes[[1L]],
    random = sample(modes, 1L)
  )
}
