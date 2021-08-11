#' Mise en forme d'un df aspe au format commun avec wama, fédés, etc.
#'
#' @param passerelle Dataframe passerelle.
#'
#' @return Un dataframe au format souhaité avec les coordonnées reprojetées.
#' @export
#'
#' @importFrom dplyr select group_by_at slice rename summarise ungroup left_join pull
#' @importFrom aspe mef_ajouter_ope_date mef_ajouter_libelle mef_ajouter_lots mef_ajouter_esp_code_alternatif
#' @importFrom aspe mef_ajouter_type_protocole mef_ajouter_operateur geo_convertir_coords_df geo_ajouter_crs
#'
#' @examples
#' \dontrun{
#' aspe <- passerelle %>%
#' clean_aspe()
#' }
#'
clean_aspe <- function(passerelle)

{
  # Création d'un df passerelle complété des infos nécessaires à la suite
  passerelle <- passerelle %>%
    mef_ajouter_ope_date() %>%
    mef_ajouter_libelle() %>%
    mef_ajouter_lots() %>%
    mef_ajouter_esp_code_alternatif() %>%
    mef_ajouter_type_protocole() %>%
    mef_ajouter_operateur() %>%
    mutate(code_exutoire = NA)

  # Liste des identifiants des points et collecte de leurs coordonnées
  mes_pops <- passerelle %>%
    pull(pop_id) %>%
    unique()

  pops <- point_prelevement %>%
    filter(pop_id %in% mes_pops) %>%
    geo_ajouter_crs(var_id_crs = "pop_typ_id") %>%
    select(pop_id,
           pop_coordonnees_x,
           pop_coordonnees_y,
           typ_code_epsg)

  # Conversion en WGS84
  coords <- geo_convertir_coords_df(
    df = pops,
    var_x = "pop_coordonnees_x",
    var_y = "pop_coordonnees_y",
    var_crs_initial = "typ_code_epsg",
    crs_sortie = 4326
  ) %>%
    rename(x_wgs84 = X,
           y_wgs84 = Y)

  # Ajout des coordonnées converties à la passerelle et renommage
  aspe <- passerelle %>%
    left_join(coords) %>%
    select(
      code_exutoire,
      code_station = sta_id,
      localisation = pop_libelle,
      x_wgs84,
      y_wgs84,
      date_peche = ope_date,
      organisme = utilisateur,
      type_peche = pro_libelle,
      code_espece = esp_code_alternatif,
      effectif = lop_effectif
    )

  # Passage en présence - absence
  aspe <- aspe %>%
    mutate(effectif = ifelse(effectif > 0, 1, 0))

  aspe

}
