#' Fréquences pour une modalité d'une variable qualitative
#'
#' Calcule l'effectif, la fréquence relative et la fréquence relative cumulée.
#'
#' @param x Vecteur (factor/character/numeric) représentant la variable.
#' @param cell Modalité à extraire (par **nom** comme "Technicien" ou par
#'   **indice** 1, 2, 3, ...). Si `NULL`, renvoie la table complète.
#' @param na_rm Logique. `TRUE` pour ignorer les NA (défaut). Si `FALSE`,
#'   une modalité "NA" sera ajoutée si nécessaire.
#' @param as_percent Logique. Si `TRUE`, les colonnes `freq` et `freq_cum`
#'   sont exprimées en pourcentage (0–100). Défaut `FALSE`.
#' @param digits Entier. Nombre de décimales à afficher quand `as_percent = TRUE`.
#'   Défaut `3`.
#'
#' @return Un `data.frame` avec les colonnes :
#'   `modalite`, `n` (effectif), `freq` (fréquence relative),
#'   `freq_cum` (fréquence relative cumulée).
#'
#' @examples
#' frequence(c("A","B","A","C","B","B"))              # tableau complet
#' frequence(c("A","B","A","C","B","B"), "B")         # modalité par nom
#' frequence(c("A","B","A","C","B","B"), 2)           # modalité par indice
#' frequence(c("A","B","A","C","B","B"), as_percent=TRUE) # en %
#'
#' @export
frequence <- function(x, cell = NULL, na_rm = TRUE,
                      as_percent = FALSE, digits = 3) {

  # Construire la table d'effectifs (gestion des NA selon na_rm)
  tbl <- if (na_rm) {
    table(x[!is.na(x)], useNA = "no")
  } else {
    table(x, useNA = "ifany")
  }

  if (length(tbl) == 0L) stop("Aucune donnée non manquante à traiter.")

  n <- as.integer(tbl)
  modalites <- names(tbl)

  total <- sum(n)
  freq <- n / total
  freq_cum <- cumsum(freq)

  df <- data.frame(
    modalite = modalites,
    n = n,
    freq = as.numeric(freq),
    freq_cum = as.numeric(freq_cum),
    stringsAsFactors = FALSE
  )

  # Option pourcentages
  if (as_percent) {
    df$freq <- round(df$freq * 100, digits)
    df$freq_cum <- round(df$freq_cum * 100, digits)
  }

  # Si aucune modalité précise demandée, renvoyer la table complète
  if (is.null(cell)) return(df)

  # Extraction par nom OU indice
  if (is.numeric(cell)) {
    # Ambiguïté possible si les modalités sont numériques :
    # on essaie d'abord par nom, sinon on prend comme indice.
    nm <- as.character(cell)
    idx <- match(nm, df$modalite)
    if (!is.na(idx)) return(df[idx, , drop = FALSE])

    if (cell < 1 || cell > nrow(df)) stop("Indice 'cell' hors bornes.")
    return(df[as.integer(cell), , drop = FALSE])
  } else {
    idx <- match(as.character(cell), df$modalite)
    if (is.na(idx)) stop("Modalité non trouvée : ", cell)
    return(df[idx, , drop = FALSE])
  }
}

