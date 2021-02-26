#' Mise d'un df brut des pêches SD au format passerelle
#'
#' @param sd_brut Dataframe données SD
#' @param crs_init Numérique. Code EPSG du CRS initial
#' @param crs_fin Numérique. Code EPSG du CRS de sortie
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
#' sd_data__propre <- sd_data_brut %>%
#' clean_sd()
#' }
clean_sd <- function(sd_brut, crs_init = 2154, crs_fin = 4326)

  {

  coords <- get_coords(sf_obj = sd_brut,
                       crs_init = crs_init,
                       crs_fin = crs_fin)

  sd <- sd_brut %>%
    st_drop_geometry() %>%
    bind_cols(coords) %>%
    pivot_longer(cols = ABH:VAX,
                 names_to = "code_espece",
                 values_to = "effectif") %>%
    mutate(code_station = NA,
           date_peche = Date,
           organisme = "SD OFB",
           type_peche = "Atlas",
           localisation = NA) %>%
    select(code_exutoire = IDD,
           code_station,
           localisation,
           x_wgs84, y_wgs84,
           date_peche,
           organisme,
           type_peche,
           code_espece,
           effectif) %>%
    mutate_at(vars(code_station, localisation, date_peche),
              as.character)

  sd


}

