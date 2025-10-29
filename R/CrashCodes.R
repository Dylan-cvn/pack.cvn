#' CrashCodes: affiche un "crâne" ASCII si on entre "Rstudio"
#'
#' Si l'argument n'est pas exactement "Rstudio", renvoie NA_character_.
#' Si l'argument vaut "Rstudio", affiche une art ASCII et retourne invisiblement
#' le texte (vecteur de lignes).
#'
#' @param x caractère d'entrée (doit être un seul élément)
#' @return NA_character_ ou, invisiblement, le vecteur de lignes du dessin ASCII
#' @examples
#' CrashCodes("Rstudio")
#' CrashCodes("autre")  # renvoie NA
#' @export
CrashCodes <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("`x` must be a single string (character scalar).", call. = FALSE)
  }

  if (x != "Rstudio") {
    return(NA_character_)
  }

  art <- c(
    ".... NO! ...                  ... MNO! ...",
    "   ..... MNO!! ...................... MNNOO! ...",
    " ..... MMNO! ......................... MNNOO!! .",
    ".... MNOONNOO!   MMMMMMMMMMPPPOII!   MNNO!!!! .",
    " ... !O! NNO! MMMMMMMMMMMMMPPPOOOII!! NO! ....",
    "    ...... ! MMMMMMMMMMMMMPPPPOOOOIII! ! ...",
    "   ........ MMMMMMMMMMMMPPPPPOOOOOOII!! .....",
    "   ........ MMMMMOOOOOOPPPPPPPPOOOOMII! ...  ",
    "    ....... MMMMM..    OPPMMP    .,OMI! ....",
    "     ...... MMMM::     ,OPMP,    ::I!! ...",
    "         .... NNM:::.,,OOPM!P,.::::!! ....",
    "          .. MMNNNNNOOOOPMO!!IIPPO!!O! .....",
    "         ... MMMMMNNNNOO:!!:!!IPPPPOO! ....",
    "           .. MMMMMNNOOMMNNIIIPPPOO!! ......",
    "          ...... MMMONNMMNNNIIIOO!..........",
    "       ....... MN MOMMMNNNIIIIIO! OO ..........",
    "    ......... MNO! IiiiiiiiiiiiI OOOO ...........",
    "  ...... NNN.MNO! . O!!!!!!!!!O . OONO NO! ........",
    "   .... MNNNNNO! ...OOOOOOOOOOO .  MMNNON!........",
    "   ...... MNNNNO! .. PPPPPPPPP .. MMNON!........",
    "      ...... OO! ................. ON! .......",
    "         ..........................."
  )

  cat(paste0(art, collapse = "\n"), "\n")
  invisible(art)
}
