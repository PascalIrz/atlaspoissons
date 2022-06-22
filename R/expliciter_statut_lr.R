#' Fonction permettant d'expliciter le statut liste rouge de chaque espèce
#'
#' @param df le dataframe
#' @param var la variable à expliciter
#'
#' @return modification du dataframe avec modification des valeurs de la variable
#' @export
#'
#' @importFrom dplyr mutate case_when
#'
#' @examples expliciter_statut_lr(df = passerelle_taxo, var = lr_nationale)
#'

expliciter_statut_lr <- function(df, var) {

  var <- enquo(var)

  df <- df %>%
    mutate(
      !!var := case_when(
        !!var == "EX" ~ "Eteint",
        !!var == "EW" ~ "Eteint à l'état sauvage",
        !!var == "CR" ~ "En danger critique d'extinction",
        !!var == "EN" ~ "En danger",
        !!var == "VU" ~ "Vulnérable",
        !!var == "NT" ~ "Quasi menacé",
        !!var == "LC" ~ "Préoccupation mineure",
        !!var == "DD" ~ "Données insuffisantes",
        (!!var == "NE" | is.na(!!var)) ~ "Non évalué"
      ))
}
