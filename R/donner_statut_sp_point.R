#'Donner le statut des especes présence/absence dans chaque station
#'
#' @param sf_data dataframe de données sf
#'
#' @return un dataframe au format souhaité avec le code station, le code espèce
#' les effectifs, le statut et le type de pêche
#' @export
#'
#' @importFrom dplyr select mutate distinct summarise
#' @importFrom sf st_drop_geometry
#'
#' @examples statut <- donner_statut_sp_point(sf_data = data)

donner_statut_sp_point <- function (sf_data) {

  data <- sf_data %>%
    sf::st_drop_geometry()

  point <- data %>%
    select(code_station,
           code_espece,
           effectif,
           type_peche,
           ope_id,
           annee) %>%
    distinct() %>%
   filter(
        type_peche %in% c("Pêche complète à un ou plusieurs passages",
                          "Complète",
                          "Pêche partielle par points (grand milieu)",
                          "Stratifiée par Points (grand milieu)",
                          "Atlas",
                          "Pêche par ambiances",
                          "Pêche partielle sur berge",
                          "Complet") |
          effectif > 0
      ) %>%
    mutate(presence = ifelse(effectif > 0,
           TRUE,
           FALSE))

}
