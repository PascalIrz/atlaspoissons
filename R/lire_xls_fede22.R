#' Fonction permettant la lecture des données de la fédé22.
#'
#' @param fichier charactère. Donner ici le fichier à lire
#'
#' @return Un dataframe à partir du fichier xls
#' @export
#'
#' @importFrom readxl read_xls
#'
#' @examples lire_xls_fede22("raw_data/donnees_fede22/FDPPMA 22_baseexcelpêches_scientifiques_OFB 2021.xls")


lire_xls_fede22 <- function(fichier) {

  # Lecture du fichier xls (pas xlsx)
  # Il y a plusieurs sheets, on précise quel sheet on veut lire
  data <- read_xls(fichier, sheet = "Données_Cours_d'eau")

  # Création du dataframe à partir de la lecture
  fede22_base <- data.frame(data)

  return(fede22_base)

}
