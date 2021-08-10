#' Mise en forme d'un df aspe au format commun avec wama, fédés, etc.
#'
#' @param passerelle Dataframe passerelle.
#'
#' @return Un dataframe au format souhaité avec les coordonnées reprojetées.
#' @export
#'
#' @importFrom dplyr select group_by_at slice rename summarise ungroup left_join
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

  passerelle <- passerelle %>%
    mef_ajouter_ope_date() %>%
    mef_ajouter_libelle() %>%
    mef_ajouter_lots() %>%
    mef_ajouter_esp_code_alternatif() %>%
    mef_ajouter_type_protocole() %>%
    mef_ajouter_operateur() %>%
    mutate(code_exutoire = NA)

  mes_pops <- passerelle %>%
    pull(pop_id) %>%
    unique()

  pops <- point_prelevement %>%
    filter(pop_id %in% mes_pops) %>%
    geo_ajouter_crs(var_id_crs = "pop_typ_id") %>%
    select(
      pop_id,
      pop_coordonnees_x,
      pop_coordonnees_y,
      typ_code_epsg
    )

  coords <- geo_convertir_coords_df(df = pops,
                                    var_x = "pop_coordonnees_x",
                                    var_y = "pop_coordonnees_y",
                                    var_crs_initial = "typ_code_epsg",
                                    crs_sortie = 2154) %>%
    rename(x_l93 = X,
           y_l93 = Y)

  aspe <- passerelle %>%
    left_join(coords) %>%
    select(code_exutoire,
           code_station = sta_id,
           localisation = pop_libelle,
           x_l93,
           y_l93,
           date_peche = ope_date,
           organisme = utilisateur,
           type_peche = pro_libelle,
           code_espece = esp_code_alternatif,
           effectif = lop_effectif)

  # df %>%
  #   select(sta_code_sandre,
  #          pop_code_sandre:proj_pop,
  #          protocole_peche,
  #          ope_date,
  #          esp_nom_latin,
  #          esp_code_sandre,
  #          lop_id,
  #          lop_effectif) %>%
  #   group_by(sta_code_sandre,
  #            pop_coordonnees_x,
  #            pop_coordonnees_y,
  #            proj_pop,
  #            protocole_peche,
  #            ope_date,
  #            esp_nom_latin,
  #            esp_code_sandre,
  #            lop_id,
  #            lop_effectif) %>%
  #     slice(1) %>%
  #     rename(effectif = lop_effectif,
  #            code_station = sta_code_sandre,
  #            type_peche = protocole_peche) %>%
   # group_by(code_station,
   #          pop_coordonnees_x,
   #          pop_coordonnees_y,
   #          proj_pop,
   #          type_peche,
   #          ope_date,
   #          esp_nom_latin,
   #          esp_code_sandre) %>%
  group_by_at(vars(-effectif)) %>%
    summarise(effectif = sum(effectif, na.rm = TRUE)) %>%
  ungroup()

}
