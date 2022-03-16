#' Assemblage des fichiers annuels
#'
#' Cette fonction permet d'assembler - en les empilant - les fichiers xlsx en homogénéisant
#'     les noms des colonnes en un unique dataframe.
#'
#' @param fichier_reference Caractère. Chemin vers le fichier excel dont les noms de colonnes vont servir
#'     de référence pour homogénéiser les noms des colonnes des différents fichiers excel
#'
#' @return Dataframe national pour l'ensemble des années de données disponibles
#' @export
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_detect
#'
#' @examples
#' \dontrun{
#' data <- lire_xlsx_fede35(repertoire="raw_data",
#' fichier_reference = "CR op pêche elec FD35 2020-VF.xlsx")
#' )
#' }
#'
lire_xlsx_fede35 <- function(repertoire, fichier_reference = "CR op pêche elec FD35 2020-VF.xlsx") {

  #Lecture d'un seul fichier excel qui range les variables dans l'ordre choisi
  lire_un_fichier <- function(fichier, ordre_var) {
    data <- read_xlsx(fichier)

    #!str_detect pour mettre la négation
    if(!str_detect("Code station", names(data)))
    {
      data <- data %>%
        mutate(`Code station` = NA)
    }

    data <- data %>%
      select(ordre_var)

    return(data)
  }

  # liste des fichiers .xlsx
  xlsx_files <- list.files(path = repertoire,
                          pattern = "CR op pêche elec FD35",
                          full.names = TRUE)

  variables <- read_xlsx(paste0(repertoire, "/", fichier_reference)) %>%
    names() %>%
  .[-1]

  # ces fichiers sont lus puis "empilés"
  # utilisation de rbind qui est plus tout-terrain que map() %>% bind_rows() ou mp_df()
  FD35 <- purrr::map(.x = xlsx_files,
                     .f = lire_un_fichier,
                     ordre_var=variables) %>%
    reduce(rbind)

  # # # renommage des variables pour enlever les caractères < et >
  # names(onde) <- names(onde) %>%
  #   str_replace_all(pattern = '[<>]', replacement = '')
  #
  # # # mise des variables au bon format
  # onde <- onde %>%
  #   mutate(Annee = as.integer(Annee),
  #          RsObservationNat = as.integer(RsObservationNat),
  #          CoordXSiteHydro = as.numeric(CoordXSiteHydro),
  #          CoordYSiteHydro = as.numeric(CoordYSiteHydro),
  #          ProjCoordSiteHydro = as.integer(ProjCoordSiteHydro))
  #
  return(FD35)

}
<<<<<<< HEAD
=======

# data <- lire_xlsx_fede35(repertoire="raw_data")
#
# xlsx_files <- list.files(path = repertoire,
#                          pattern = "CR op pêche elec FD35",
#                          full.names = TRUE)
>>>>>>> 32779907aee4efb35c042cc06e94e4cea659a1f2
