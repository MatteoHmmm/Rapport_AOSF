library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(sf)

# 1. Acquisition
if (!dir.exists("data")) dir.create("data")
url <- "https://www.data.gouv.fr/fr/datasets/r/98f3161f-79ff-4f16-8f6a-6d571a80fea2"
lignes <- readLines(url)
lignes <- lignes[!str_detect(lignes, "^#|^finess;etalab")]

# 2. Structure
df_structure <- tibble(ligne = lignes[str_detect(lignes, "^structureet")]) %>%
  separate(ligne, into = c(
    "type", "nofinesset", "nofinessej", "rs", "rslongue", "complrs",
    "compldistrib", "numvoie", "typvoie", "voie", "compvoie", "lieuditbp",
    "commune", "departement", "libdepartement", "ligneacheminement",
    "telephone", "telecopie", "categetab", "libcategetab", "categagretab",
    "libcategagretab", "siret", "codeape", "codemft", "libmft",
    "codesph", "libsph", "dateouv", "dateautor", "datemaj", "numuai"
  ), sep = ";", fill = "right", extra = "drop")

# 3. Géo 
df_geo <- tibble(ligne = lignes[str_detect(lignes, "^geolocalisation")]) %>%
  separate(ligne, into = c("type_suppr", "nofinesset", "coordxet", "coordyet", "sourcecoordet", "datemaj_geo"), 
           sep = ";", fill = "right", extra = "drop") %>%
  mutate(across(c(coordxet, coordyet), as.numeric)) %>%
  filter(!is.na(coordxet), !is.na(coordyet))

# 4. Fusion et Conversion
df_final <- df_structure %>%
  inner_join(df_geo, by = "nofinesset") %>%
  st_as_sf(coords = c("coordxet", "coordyet"), crs = 2154, remove = FALSE) %>%
  st_transform(crs = 4326) %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2],
    annee = as.numeric(substr(datemaj, 1, 4))
  ) %>%
  st_drop_geometry() %>%
  select(-type_suppr, -datemaj_geo) 

# 5. Sauvegarde et Nettoyage
#write_csv(df_final, "data/finess_geolocalise.csv")
finess_data <- df_final


rm(list = setdiff(ls(), c("finess_data","ameli_effectifs")))
