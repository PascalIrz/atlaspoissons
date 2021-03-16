#' Recodage des codes espèces et exclusion de taxons
#'
#' Par exemple les carpes cuir, miroir, etc. sont regroupées sous un unique code CCX.
#' Idem pour les vandoises en VAX.
# 'Dans l'ouest Finistère, il n'y a que de l'épinoche => recodage de l'épinochette sur cette zone.
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

  df %>%
    mutate(code_espece = str_replace(code_espece, pattern = "CCU", replacement = "CCX"),
           code_espece = str_replace(code_espece, pattern = "CMI", replacement = "CCX"),
           code_espece = str_replace(code_espece, pattern = "CCO", replacement = "CCX"),
           code_espece = str_replace(code_espece, pattern = "CAG", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "CAD", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "CAA", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "CAS", replacement = "CAX"),
           code_espece = str_replace(code_espece, pattern = "VAN", replacement = "VAX"),
           code_espece = str_replace(code_espece, pattern = "VAR", replacement = "VAX"),
           code_espece = ifelse(code_espece == "EPT" & x_wgs84 < (-4.1), "EPI", code_espece))
}
