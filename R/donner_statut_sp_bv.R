#'Donner le statut des especes présence/absence dans chaque bassin versant, indiquant en plus si
#'l'absence est avérée ou est due à un échantillonnage insuffisant.
#'
#' @param sf_data dataframe de données sf
#'
#' @return un dataframe au format souhaité avec le code station, le code espèce et le statut
#' @export
#'
#' @importFrom dplyr select mutate distinct summarise
#' @importFrom sf st_drop_geometry
#'
#' @examples statut_bv <- donner_statut_sp_bv(sf_data = data)

donner_statut_sp_bv <- function (sf_data) {

  data <- sf_data %>%
    sf::st_drop_geometry()

  #On commence par trier les peches étant des inventaires
  bv <- data %>%
    select(code_exutoire,
           type_peche,
           code_station,
           esp_nom_commun) %>%
    distinct() %>%
    mutate(
      statut = ifelse(
        type_peche %in% c("Pêche complète à un ou plusieurs passages",
                          "Complète",
                          "Pêche partielle par points (grand milieu)",
                          "Stratifiée par Points (grand milieu)",
                          "Atlas",
                          "Pêche par ambiances",
                          "Pêche partielle sur berge",
                          "Complet"),
        TRUE,
        FALSE
      )
    ) %>%
    group_by(code_exutoire) %>%
    summarise(statut = max(statut))

  # On joint les données
  data_bv <- data %>%
    left_join(bv)

  # On termine par créer la liste des statuts
  bv_statuts <- data_bv %>%
    group_by(code_exutoire, code_espece) %>%
    summarise(statut = case_when(
      max(effectif) > 0 ~ "Présence",
      max(effectif) == 0 & max(statut) == 1 ~ "Absence avérée",
      max(effectif) == 0 & max(statut) == 0 ~ "Absence échantillonnage insuffisant",
      TRUE ~ "Non prospecté"
    ))

}
