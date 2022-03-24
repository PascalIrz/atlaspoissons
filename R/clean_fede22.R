#' Fonction permettant de nettoyer le dataframe brut obtenu avec la fonction
#' lire_xls_fede22() en l'homogénéisant par rapport aux autres dataframes.
#'
#' @param df_brut Caractère. Fichier désiré à nettoyer.
#' @param crs_init Ne pas renseigner. Crs initial du fichier, déjà renseigné.
#' @param crs_fin Ne pas renseigner. Crs final du fichier, déjà renseigné.
#'
#' @return Un dataframe homogénéisé avec les autres dataframes des autres données
#' de pêche
#' @export
#'
#' @importFrom dplyr mutate select mutate_at vars if_else rename
#'
#' @examples clean_fede22(fede22_base)

clean_fede22 <- function(df_brut, crs_init = 2154, crs_fin = 4326) {

  # Crée de nouvelles colonnes avec les informations et noms désirés
  df <- df_brut %>%
    mutate(
      code_exutoire = NA,
      code_station = NA,
      date_peche = Date,
      date_peche = if_else(is.na(date_peche),
                           min(date_peche, na.rm = TRUE),
                           date_peche),
      annee = str_sub(date_peche, 1, 4),
      annee = as.integer(annee),
      source_donnee = "Fede 22",
      code_espece = `Espèce`,
      effectif = Nb.Individus,
      type_peche = Type,
      localisation = Cours.d.eau)

  # Sélectionne les colonnes à garder
  df <- df %>%
    select(
      code_exutoire,
      code_station,
      localisation,
      X,
      Y,
      date_peche,
      annee,
      source_donnee,
      type_peche,
      code_espece,
      effectif) %>%
    mutate_at(vars(code_station, localisation, date_peche),
              as.character)

  # Tranforme les coordonnées L93 en WGS84
  coords <- df %>%
    st_as_sf(coords = c("X", "Y"),
             crs = crs_init) %>%
    st_transform(crs = crs_fin) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(x_wgs84 = X,
           y_wgs84 = Y)

  df <- df %>%
    cbind(coords) %>%
    select(code_exutoire:localisation,
           x_wgs84,
           y_wgs84,
           date_peche:effectif)

  df

}
