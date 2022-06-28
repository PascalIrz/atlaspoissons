#' Fonction permettant la lecture des données de la fédé22.
#'
#' @param fichier charactère. Donner ici le fichier à lire
#'
#' @return Un dataframe à partir du fichier xls
#' @export
#'
#' @importFrom readxl read_xls
#'
#' @examples
#' \dontrun{
#' data_f22 <- lire_xls_fede22("raw_data/donnees_fede22/FDPPMA_22_peches_scientifiques_2021.xls")
#' }
#'
#' @examples


lire_xls_fede22 <- function(fichier) {

  # Lecture du fichier xls (pas xlsx)
  # Il y a plusieurs sheets, on précise quel sheet on veut lire
  data <- read_xls(fichier, sheet = "Données_Cours_d'eau")
  # Problème cellule A162: On devrait avoir un "22" pour le département mais à la place on a ###.
  # Mais on s'en moque parce qu'on n'a pas besoin du département pour la suite.

  # Création du dataframe à partir de la lecture
  fede22_base <- data.frame(data)

  return(fede22_base)

}

