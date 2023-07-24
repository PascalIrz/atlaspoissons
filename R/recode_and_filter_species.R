#' Recodage des codes espèces et exclusion de taxons
#'
#' Par exemple les carpes cuir, miroir, etc. sont regroupées sous un unique code CCX.
#' Idem pour les vandoises en VAX.
#' Dans l'ouest Finistère, il n'y a que de l'épinoche => recodage de l'épinochette sur cette zone.
#' Tous les carassins sont des argentés.
#'
#' Cette fonction requiert le jeu de données "data_passerelle_taxo" du package {aspe}.
#'
#' @param df Dataframe standardisé contenant les données.
#' @param sp_to_remove Vecteur texte contenant les codes à trois lettres des taxons à
#'     supprimer.
#'
#' @return Le dataframe mis à jour.
#' @export
#'
#' @importFrom stringr str_replace
#' @importFrom dplyr filter mutate case_when pull
#'
#' @examples
#' \dontrun{
#' df_propre <- df_brut %>%
#' recode_and_filter_species(sp_to_remove = c("OCV", "ASA"))
#' }
recode_and_filter_species <- function(df, sp_to_remove = NULL) {
  if (!is.null(sp_to_remove))

  {
    df <- df %>%
      filter(!code_espece %in% sp_to_remove)
  }

  df <- df %>%
    mutate(
      code_espece = case_when(
        code_espece == "CCU" ~ "CCO",
        code_espece == "CCX" ~ "CCO",
        code_espece == "CMI" ~ "CCO",
        code_espece == "CAS" ~ "CAG",
        code_espece == "CAD" ~ "CAG",
        code_espece == "CAA" ~ "CAG",
        code_espece == "CAX" ~ "CAG",
        code_espece == "VAN" ~ "VAR",
        code_espece == "EPT" & x_wgs84 < (-4.1) ~ "EPI",
        TRUE ~ code_espece
      )
    )

  # Permet de sélectionner les codes espèces dans le tableau "data_passerelle_taxo"
  # disponible dans le package aspe. On ne sélectionne que les codes "valides",
  # c'est à dire les codes de 3 lettres.
  code_valide <- data_passerelle_taxo %>%
    filter(nchar(esp_code_alternatif) == 3) %>%
    pull(esp_code_alternatif) %>%
    unique()

  # Filtrer les espèces valides
  df %>%
    filter(code_espece %in% code_valide)

}
