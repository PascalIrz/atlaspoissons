#' Mise d'un df brut aspe au format passerelle
#'
#' @param df Dataframe contenant les données.
#'
#' @return Un dataframe au format souhaité avec les coordonnées reprojetées
#' @export
#'
#' @importFrom dplyr select group_by slice rename summarise ungroup
#'
#' @examples
#' \dontrun{
#' aspe_propre <- aspe_brut %>%
#' clean_aspe()
#' }
clean_aspe <- function(df)

  {

  df %>%
    select(sta_code_sandre,
           pop_code_sandre:proj_pop,
           protocole_peche,
           ope_date,
           esp_nom_latin,
           esp_code_sandre,
           lop_id,
           lop_effectif) %>%
    group_by(sta_code_sandre,
             pop_coordonnees_x,
             pop_coordonnees_y,
             proj_pop,
             protocole_peche,
             ope_date,
             esp_nom_latin,
             esp_code_sandre,
             lop_id,
             lop_effectif) %>%
      slice(1) %>%
      rename(effectif = lop_effectif,
             code_station = sta_code_sandre,
             type_peche = protocole_peche) %>%
   group_by(code_station,
            pop_coordonnees_x,
            pop_coordonnees_y,
            proj_pop,
            type_peche,
            ope_date,
            esp_nom_latin,
            esp_code_sandre) %>%
    summarise(effectif = sum(effectif, na.rm = TRUE)) %>%
  ungroup()

}
