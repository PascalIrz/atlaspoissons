#' Mise d'un df brut des pêches fédés au format passerelle
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
#'
#' @examples
#' \dontrun{
#' fede_propre <- fede_brut %>%
#' clean_fede()
#' }
clean_fede <- function(df_brut,
                       crs_init = 2154,
                       crs_fin = 4326)

{
  coords <- get_coords(sf_obj = df_brut,
                       crs_init = crs_init,
                       crs_fin = crs_fin)

  df <- df_brut %>%
    st_drop_geometry() %>%
    bind_cols(coords) %>%
    pivot_longer(cols = ABH:VAX,
                 names_to = "code_espece",
                 values_to = "effectif") %>%
    mutate(
      code_station = NA,
      date_peche = NA,
      annee = NA,
      localisation = NA,
      organisme = "Fede 56"
    ) %>%
    select(
      code_exutoire = IDD,
      code_station,
      localisation,
      x_wgs84,
      y_wgs84,
      date_peche,
      annee,
      organisme,
      type_peche = Ctxte_Pech,
      code_espece,
      effectif
    ) %>%
    mutate_at(vars(code_station, localisation, date_peche),
              as.character) %>%
    filter(!is.na(code_exutoire)) # 2 observations sans IDD ont des coordonnées aberrantes

  df

}

