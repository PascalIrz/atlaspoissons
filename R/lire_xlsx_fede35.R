#' Assemblage des fichiers annuels
#'
#' Cette fonction permet d'assembler - en les empilant - les fichiers xlsx en homogénéisant
#'     les noms des colonnes en un unique dataframe.
#'
#' @param repertoire Caractère. Chemin vers le répertoire où se trouvent les fichiers Excel de données.
#' @param fichier_reference Caractère. Nom du fichier excel dont les noms de colonnes vont servir
#'     de référence pour homogénéiser les noms des colonnes des différents fichiers excel
#'
#' @return Dataframe pour l'ensemble des années de données disponibles par la fédé 35.
#' @export
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate case_when filter group_by across all_of vars
#' @importFrom tidyr fill
#'
#' @examples
#' \dontrun{
#' data <- lire_xlsx_fede35(repertoire="raw_data",
#' fichier_reference = "CR op pêche elec FD35 2020-VF.xlsx")
#' )
#' }
#'
lire_xlsx_fede35 <- function(repertoire, fichier_reference = "CR op pêche elec FD35 2020-VF.xlsx") {

  # Fonction de lecture d'un seul fichier excel qui range les variables dans l'ordre choisi
  # ----------------------- début fonction
  lire_un_fichier <- function(fichier, ordre_var) {

    data <- read_xlsx(fichier) %>%
      filter(!is.na(`Espèce`) & !is.na(`Nb Individus`)) %>% # fautes de frappe à corriger
      mutate(`Lieu dit` = case_when(`Lieu dit` == "La Démonnais" ~ "La Démonais",
                                    `Lieu dit` == "Moulin Ean" ~ "Moulin Eon",
                                    TRUE ~ `Lieu dit`))

    #!str_detect pour mettre la négation
    if(!str_detect("Code station", names(data)))
    {
      data <- data %>%
        mutate(`Code station` = NA)
    }

    # gestion des valeurs manquantes, mise dans l'ordre puis gestion formats
    # NB pour ce qui est des valeurs manquantes en l'absence de code station il y a risque de confusion
    # en cas de lieux dits homonymes
    data <- data %>%
      group_by(across(Dep:Y)) %>%
      fill(Date) %>%
      group_by(`Lieu dit`, Commune) %>%
      fill(X, Y, .direction = "downup") %>%
      group_by(`Lieu dit`) %>%
      fill(X, Y, .direction = "downup") %>%
      select(all_of(ordre_var)) %>%
      mutate_at(vars(Y, `Nb Individus`), as.numeric) %>%
      mutate(Date = as.Date(Date))

    return(data)

  }
  # ----------------------- fin fonction lire_un_fichier()

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
                     ordre_var = variables) %>%
    reduce(rbind) %>%
    group_by(`Lieu dit`) %>%
    fill(X, Y, .direction = "downup") %>%  # remplissage des dernières valeurs manquantes
    ungroup()


  return(FD35)

}
