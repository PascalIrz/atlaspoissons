#' Mise en forme d'un df aspe au format d'échange commun avec wama, fédés, etc.
#'
#' @param df_brut Dataframe agence
#'
#' @return Un dataframe au format souhaité.
#' @export
#'
#' @importFrom dplyr mutate select mutate_at group_by summarise ungroup left_join
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry
#' @importFrom magrittr set_colnames
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' agence_propre <- agence_brut %>%
#' clean_agence()
#' }
clean_agence <- function(df_brut)

{
  captures <- df_brut %>%
    mutate(code_exutoire = NA,
           organisme = "EALB") %>%
    select(
      code_exutoire,
      code_station = CdStationMesureEauxSurface,
      localisation = NomEntiteHydrographique,
      date_peche = Op_Dtd,
      organisme,
      type_peche = L1_Li_Nom,
      ABH:VAR
    ) %>%
    mutate(date_peche = as.character(date_peche)) %>%
    mutate_at(vars(ABH:VAR), replace_na, 0L) %>%
    pivot_longer(cols = ABH:VAR,
                 names_to = "code_espece",
                 values_to = "effectif")

  stations_agence <- df_brut %>%
    select(code_station = CdStationMesureEauxSurface,
           x_l93 = CoordXPointEauxSurf,
           y_l93 = CoordYPointEauxSurf) %>%
    group_by(code_station) %>%
    summarise(x_l93 = mean(x_l93, na.rm = TRUE),
              y_l93 = mean(y_l93, na.rm = TRUE)) %>%
    ungroup() %>%
    sf::st_as_sf(coords = c("x_l93", "y_l93"),
                 crs = 2154) %>%
    st_transform(crs = 4326)

  coords <- stations_agence %>%
    st_coordinates() %>%
    as.data.frame() %>%
    magrittr::set_colnames(c("x_wgs84", "y_wgs84"))

  stations_agence <- cbind(stations_agence, coords) %>%
    st_drop_geometry()

  agence <- left_join(x = captures,
                      y = stations_agence) %>%
    select(
      code_exutoire,
      code_station,
      localisation,
      x_wgs84,
      y_wgs84,
      date_peche,
      organisme,
      type_peche,
      code_espece,
      effectif
    )
}
