#' Mise d'un df brut des pêches SD au format passerelle
#'
#' @param df_brut Dataframe données SD.
#' @param crs_init Numérique. Code EPSG du CRS initial. Par défaut c'est 2154 (Lambert 93).
#' @param crs_fin Numérique. Code EPSG du CRS de sortie. Par défaut c'est 4326 (WGS84).
#'
#' @return Un dataframe au format souhaité avec les coordonnées reprojetées
#' @export
#'
#' @importFrom dplyr bind_cols mutate select mutate_at vars
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub str_replace_all
#'
#' @examples
#' \dontrun{
#' sd_data_propre <- sd_data_brut %>%
#' clean_sd()
#' }
clean_fede <- function(df_brut,
                       crs_init = 2154,
                       crs_fin = 4326)

{
  df_brut <- df_brut %>%
    filter(!is.na(x_l93), !is.na(y_l93))

  df_brut_sf <- df_brut %>%
    sf::st_as_sf(coords = c("x_l93", "y_l93"),
                 crs = crs_init)

  coords <- get_coords(sf_obj = df_brut_sf,
                       crs_init = crs_init,
                       crs_fin = crs_fin)

  df <- df_brut %>%
    bind_cols(coords) %>%
    mutate(code_exutoire = NA,
           code_point = NA_integer_,
           annee = lubridate::year(date_ope)) %>%
    select(code_exutoire,
           code_station = code_station_interne_fede,
           code_point,
           localisation = nom_station_fede,
           x_wgs84,
           y_wgs84,
           date_peche = date_ope,
           annee,
           source_donnee = maitre_ouvrage,
           type_peche = protocole,
           code_espece = taxon,
           effectif)

  df

}
