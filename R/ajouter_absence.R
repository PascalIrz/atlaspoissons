#' Fonction permettant d'ajouter les absence des espèces pour chaque opération
#'
#' @param df fichier de donnees qui doit contenir une variable "code_espece" mais pas les
#'    noms latin ou commun, ainsi que la variable "effectif".
#'
#' @return Le dataframe df complété avec les combinaisons manquantes entre les codes espèces et
#'     les autres variables.
#' @export
#'
#' @importFrom dplyr mutate filter pull left_join
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @examples
#' \dontrun{
#' data_complete <- ajouter_absence(data)
#' }

ajouter_absence <- function(df) {

  test <- df %>%
    pivot_wider(names_from = "code_espece",
                values_from = "effectif",
                values_fill = 0) %>%
    pivot_longer(cols = -(code_exutoire:ope_id),
                 names_to = "code_espece",
                 values_to = "effectif") %>%
    filter(effectif == 0)

  test

}
