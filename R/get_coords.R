#' Collecte et reprojection des coordonnées
#' @param sf_obj Objet de classe sf dont il s'agit de collecter les coordonnées.
#' @param crs_init Numérique. Code EPSG du système de coordonnées de cet objet.
#' @param crs_fin Numérique. Code EPSG du système de coordonnées de sortie. Par défaut, 4326
#'     pour une sortie en WGS84.
#' @param col_names Vecteur caractères de deux éléments pour nommer les colonnes de coordonnées.
#'     Par défaut elles sont nommées "x_wgs84" et "y_wgs84".
#'
#' @return Un dataframe de deux colonnes avec les coordonnées dans le CRS de sortie.
#' @export
#'
#' @importFrom sf st_crs st_transform st_coordinates
#' @importFrom magrittr set_colnames '%>%'
#'
#' @examples
#' \dontrun{
#' # Les coordonnees de la DR OFB Bretagne
#' x <- 304660
#' y <- 2353600
#'
#' # Construction de l'objet sf correspondant
#' dr_ofb <- sf::st_sfc(sf::st_point(c(x,y)))
#' sf::st_crs(dr_ofb) <- 27572
#' dr_ofb
#'
#' # Coordonnees de la DR en WGS84
#' get_coords(dr_ofb, crs_init = 27572)
#' }
get_coords <- function(sf_obj, crs_init, crs_fin = 4326, col_names = c("x_wgs84", "y_wgs84"))

  {

  sf::st_crs(sf_obj) <- crs_init

  sf_obj %>%
    st_transform(crs = crs_fin) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    magrittr::set_colnames(col_names)

  }
