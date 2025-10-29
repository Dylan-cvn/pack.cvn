#' Moyenne d'une variable par catégorie
#'
#' Calcule la moyenne d'une variable numérique pour chaque modalité d'une
#' variable catégorielle, en ignorant les NA.
#' Usage : MoyenneParCategorie(Note, par = Couleur, dans = Exercice_Serie_2)
#'
#' @param var   colonne numérique à résumer (non-quotée), ex. Note
#' @param par   colonne de groupement (non-quotée), ex. Couleur
#' @param dans  data.frame (non-quoté), ex. Exercice_Serie_2
#' @return data.frame (une ligne par catégorie, colonne moy_*)
#' @export
MoyenneParCategorie <- function(var, par, dans) {
  var_expr  <- substitute(var)
  par_expr  <- substitute(par)
  data_expr <- substitute(dans)

  df <- eval(data_expr, parent.frame())
  if (!is.data.frame(df)) stop("`dans` doit être un data.frame.", call. = FALSE)

  var_name <- deparse(var_expr)
  par_name <- deparse(par_expr)

  if (!var_name %in% names(df)) stop("Colonne `", var_name, "` introuvable.", call. = FALSE)
  if (!par_name %in% names(df)) stop("Colonne `", par_name, "` introuvable.", call. = FALSE)
  if (!is.numeric(df[[var_name]])) stop("`", var_name, "` doit être numérique.", call. = FALSE)

  f_mean_na <- function(x) mean(x, na.rm = TRUE)
  out <- stats::aggregate(stats::as.formula(paste(var_name, "~", par_name)),
                          data = df, FUN = f_mean_na)
  names(out) <- c(par_name, paste0("moy_", var_name))
  out
}
