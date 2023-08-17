#' Mise en forme d'un df aspe au format d'échange commun avec aspe, fédés, etc.
#'
#' @param df_brut Dataframe wama.
#' @param crs_init Numérique. Code EPSG du CRS initial. Par défaut c'est 2154 (Lambert 93).
#' @param crs_fin Numérique. Code EPSG du CRS de sortie. Par défaut c'est 4326 (WGS84).
#'
#' @return Un dataframe au format souhaité avec les coordonnées reprojetées.
#' @export
#'
#' @importFrom dplyr bind_cols filter mutate select mutate_at vars
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' wama_propre <- wama_brut %>%
#' clean_wama()
#' }
clean_wama <- function(df_brut,
                       crs_init = 2154,
                       crs_fin = 4326)

{

  coords <- get_coords(sf_obj = df_brut,
                       crs_init = crs_init,
                       crs_fin = crs_fin)

  df <- df_brut %>%
    bind_cols(coords) %>% # ajout des coordonnées en wgs84
    st_drop_geometry() %>% # suppression de la colonnes géométrie
    dplyr::filter(!stringr::str_detect(CD_STAT, pattern = "Total")) %>% # suppression du total
    pivot_longer(cols = ABH:VAX,
                 names_to = "code_espece",
                 values_to = "effectif") %>%
    mutate(
      date_peche = NA,
      annee = as.integer(stringr::str_sub(CD_STAT, -4, -1)),
      code_station = stringr::str_sub(CD_STAT, 1,-6),
      code_point = NA_integer_,
      source_donnee = "WAMA",
      type_peche = "WAMA",
      localisation = NA
    ) %>%
    select(
      code_exutoire = IDD,
      code_station,
      code_point,
      localisation,
      x_wgs84 = X,
      y_wgs84 = Y,
      date_peche,
      annee,
      source_donnee,
      type_peche,
      code_espece,
      effectif
    ) %>%
    mutate_at(vars(code_station, localisation, date_peche),
              as.character)

  df

}

