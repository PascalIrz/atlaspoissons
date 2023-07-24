#' Fonction permettant la lecture des données des fédés départementales
#'
#' @param chemin Caractère. Chemin vers le fichier de xonnées
#' @param depts Vecteur caractères. Indique les départements à premdre en compte,
#'     c'est-à-dire les noms des onglets du fichier de données.
#'
#' @return Le dataframe
#' @export
#'
#' @importFrom purrr map_df
#'
#' @examples
#' \dontrun{
#' fedes <- lire_fichier_fedes(chemin = "raw_data/fedes_departementales_peche.xlsx")
#' }
lire_fichier_fedes <-
  function(chemin, depts = c("22", "35", "29", "56"))

  {
    lire_une_fede <- function(num_dept) {
      readxl::read_xlsx(
        path = chemin,
        sheet = num_dept,
        #  skip = 1,
        col_names = c(
          "id",
          "uuid",
          "producteur",
          "maitre_ouvrage",
          "bassin",
          "cours_deau",
          "code_station_interne_fede",
          "code_station_sandre",
          "nom_station_fede",
          "nom_station_sandre",
          "x_l93",
          "y_l93",
          "date_ope",
          "taxon",
          "protocole",
          "presence_de_lespece",
          "effectif",
          "commentaire",
          "id_jeu_donnees"
        ),
        col_types = c(
          rep("numeric", 2),
          rep("text", 8),
          rep("numeric", 2),
          "date",
          rep("text", 2),
          rep("numeric", 2),
          rep("text", 2)
        )
      ) %>%
        slice(-1) %>%  # pas moyen avec skip car dans certains depts ça génère une colonne vide
        mutate(dept = num_dept,
               date_ope = as.character(date_ope))
        }

    donnees_fedes <- map_df(.x = depts,
                            .f = lire_une_fede)

    return(donnees_fedes)
  }
