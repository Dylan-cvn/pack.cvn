#' Médiane d'une variable par catégorie
#'
#' Calcule la médiane d'une variable numérique pour chaque modalité d'une
#' variable catégorielle, en ignorant systématiquement les valeurs manquantes.
#' Usage naturel : MedianeParCategorie(Note, par = Couleur, dans = Exercice_Serie_2)
#'
#' @param var   Colonne numérique à résumer (non-quotée), ex. Note
#' @param par   Colonne de groupement (non-quotée), ex. Couleur
#' @param dans  data.frame contenant les colonnes (non-quoté), ex. Exercice_Serie_2
#'
#' @return data.frame avec une ligne par catégorie et la médiane correspondante
#'         (colonne nommée \code{med_<var>}).
#'
#' @examples
#' # Médiane des notes par couleur
#' # MedianeParCategorie(Note, par = Couleur, dans = Exercice_Serie_2)
#'
#' @export
MedianeParCategorie <- function(var, par, dans) {
  var_expr  <- substitute(var)
  par_expr  <- substitute(par)
  data_expr <- substitute(dans)

  df <- eval(data_expr, parent.frame())
  if (!is.data.frame(df)) stop("`dans` doit être un data.frame.", call. = FALSE)

  var_name <- deparse(var_expr)
  par_name <- deparse(par_expr)

  if (!var_name %in% names(df)) stop("Colonne `", var_name, "` introuvable dans `dans`.", call. = FALSE)
  if (!par_name %in% names(df)) stop("Colonne `", par_name, "` introuvable dans `dans`.", call. = FALSE)
  if (!is.numeric(df[[var_name]])) stop("`", var_name, "` doit être numérique.", call. = FALSE)

  f_median_na <- function(x) stats::median(x, na.rm = TRUE)

  out <- stats::aggregate(stats::as.formula(paste(var_name, "~", par_name)),
                          data = df, FUN = f_median_na)
  names(out) <- c(par_name, paste0("med_", var_name))
  out
}
