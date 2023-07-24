#' Reprojeter un df aspe
#'
#' @param aspe_df Dataframe Aspe issu de clean_aspe().
#' @param coords Vecteur contenant les noms des colonnes de coordonnées, X puis Y dans aspe_df.
#' @param crs_init Numérique. Code EPSG du CRS initial.
#' @param crs_fin Numérique. Code EPSG du CRS de sortie. Par défaut c'est 4326 (WGS84).
#' @param coord_names Vecteur contenant les noms des colonnes de coordonnées, X puis Y en sortie.
#'
#' @return Le dataframe avec les coordonnées reprojetées en colonnes
#' @export
#'
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom dplyr bind_cols
#' @importFrom magrittr set_colnames
#'
#' @examples
#' \dontrun{
#' aspe_l2 <- aspe_occurence %>%
#' filter(proj_pop == "Lambert II Etendu") %>%
#' transform_crs(crs_init = 27572, crs_fin = 4326)
#' }
transform_crs <- function(aspe_df,
                          coords = c("pop_coordonnees_x", "pop_coordonnees_y"),
                          crs_init,
                          crs_fin = 4326,
                          coord_names = c("x_wgs84", "y_wgs84"))

  {

  prov <- aspe_df %>%
    sf::st_as_sf(coords = coords, crs = crs_init) %>%
    st_transform(crs = crs_fin)

  coords <- st_coordinates(prov) %>%
    as.data.frame() %>%
    magrittr::set_colnames(coord_names)

  bind_cols(prov, coords)

}
