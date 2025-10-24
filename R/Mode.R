#' Mode statistique
#' @param x vecteur
#' @param na_rm TRUE pour ignorer les NA
#' @param ties "all","first","random"
#' @return valeur(s) modale(s)
#' @export
Mode <- function(x, na_rm = TRUE, ties = c("all","first","random")) {
  ties <- match.arg(ties)
  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA)
  tb <- table(x, useNA = "no")
  modes <- names(tb)[tb == max(tb)]
  modes <- type.convert(modes, as.is = TRUE)
  switch(ties,
         all    = modes,
         first  = modes[[1]],
         random = sample(modes, 1))
}
