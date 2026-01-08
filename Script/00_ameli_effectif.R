library(arrow)

#1. augmentation du timeout
options(timeout = 300)

# 2. Construction de l'URL avec les filtres intégrés
# On utilise URLencode pour gérer les espaces et caractères spéciaux
base_url <- "https://data.ameli.fr/api/explore/v2.1/catalog/datasets/demographie-effectifs-et-les-densites/exports/parquet"

requete <- paste0(
  "libelle_sexe != 'tout sexe' ",
  "and libelle_sexe != 'sexe inconnu' ",
  "and libelle_departement != 'FRANCE' ",
  "and libelle_departement != 'Tout département' ",
  "and libelle_classe_age != 'Tout âge' ",
  "and libelle_classe_age != 'âge inconnu' ",
  "and not profession_sante like 'Ensemble%'" 
)

url_filtree <- paste0(base_url, "?where=", URLencode(requete))

# 3. Importation 
ameli_effectifs <- read_parquet(url_filtree)