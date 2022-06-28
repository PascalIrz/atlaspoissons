#' Recodage des codes espèces et exclusion de taxons
#'
#' Par exemple les carpes cuir, miroir, etc. sont regroupées sous un unique code CCX.
#' Idem pour les vandoises en VAX.
#' Dans l'ouest Finistère, il n'y a que de l'épinoche => recodage de l'épinochette sur cette zone.
#' Tous les carassins sont des argentés.
#'
#' @param df Dataframe standardisé contenant les données.
#' @param sp_to_remove Vecteur texte contenant les codes à trois lettres des taxons à
#'     supprimer.
#'
#' @return Le dataframe mis à jour.
#' @export
#'
#' @importFrom stringr str_replace
#'
#' @examples
#' \dontrun{
#' df_propre <- df_brut %>%
#' recode_and_filter_species(sp_to_remove = c("OCV", "ASA"))
#' }
recode_and_filter_species <- function(df, sp_to_remove = NA) {

  if(!is.na(sp_to_remove))

  {

  df <- df %>%
    filter(!code_espece %in% sp_to_remove)
  }

  df <- df %>%
    mutate(code_espece = str_replace(code_espece, pattern = "CCU", replacement = "CCO"),
           code_espece = str_replace(code_espece, pattern = "CCX", replacement = "CCO"),
           code_espece = str_replace(code_espece, pattern = "CMI", replacement = "CCO"),
           code_espece = str_replace(code_espece, pattern = "CAS", replacement = "CAG"),
           code_espece = str_replace(code_espece, pattern = "CAD", replacement = "CAG"),
           code_espece = str_replace(code_espece, pattern = "CAA", replacement = "CAG"),
           code_espece = str_replace(code_espece, pattern = "CAX", replacement = "CAG"),
           code_espece = str_replace(code_espece, pattern = "VAN", replacement = "VAR"),
           code_espece = ifelse(code_espece == "EPT" & x_wgs84 < (-4.1), "EPI", code_espece))

  # Permet de sélectionner les codes espèces dans le fichier "passerelle_taxo"
  # disponible dans le package aspe. On ne sélectionne que les codes "valides",
  # c'est à dire les codes de 3 lettres.
  code_valide <- passerelle_taxo %>%
    filter(nchar(esp_code_alternatif) == 3) %>%
    pull(esp_code_alternatif)

  # Filtrer les espèces valides
  df %>%
    filter(code_espece %in% code_valide)

}
