# 1. Préparation de l'environnement
library(dplyr)
library(tidyr)
library(lubridate)

# 2. Traitement des données FINESS 
finess_data <- finess_data %>%
  mutate(
    departement = case_when(
      departement == "9A" ~ "971", # Guadeloupe
      departement == "9B" ~ "972", # Martinique
      departement == "9C" ~ "973", # Guyane
      departement == "9D" ~ "974", # La Réunion
      departement == "9E" ~ "975", # Saint-Pierre-et-Miquelon
      departement == "9F" ~ "976", # Mayotte
      TRUE ~ departement
    )
  )

# Calcul du nombre d'établissements par département
nb_etablissements <- finess_data %>%
  filter(!is.na(departement)) %>%
  group_by(departement) %>%
  summarise(
    nb_etablissements = n(),
    nb_etablissements_log = log(1 + n()),
    .groups = "drop"
  )

# 3. Traitement des données AMELI 
df_ameli <- ameli_effectifs %>%
  mutate(age_centre = case_when(
    libelle_classe_age == "moins de 25 ans" ~ 22,
    libelle_classe_age == "de 25 à 29 ans"  ~ 27,
    libelle_classe_age == "de 30 à 34 ans"  ~ 32,
    libelle_classe_age == "de 35 à 39 ans"  ~ 37,
    libelle_classe_age == "de 40 à 44 ans"  ~ 42,
    libelle_classe_age == "de 45 à 49 ans"  ~ 47,
    libelle_classe_age == "de 50 à 54 ans"  ~ 52,
    libelle_classe_age == "de 55 à 59 ans"  ~ 57,
    libelle_classe_age == "de 60 à 64 ans"  ~ 62,
    libelle_classe_age == "de 65 à 69 ans"  ~ 67,
    libelle_classe_age == "70 ans et plus"  ~ 75,
    TRUE ~ NA_real_
  )) %>%
  group_by(annee, departement) %>%
  summarise(
    effectif_total   = sum(effectif, na.rm = TRUE),
    effectif_total_log = log(1 + sum(effectif, na.rm = TRUE)),
    
    # Part des femmes
    part_femmes      = sum(effectif[libelle_sexe == "femmes"], na.rm = TRUE) / sum(effectif, na.rm = TRUE),
    
    # Part des 60 ans et plus
    part_60_plus     = sum(effectif[libelle_classe_age %in% c("de 60 à 64 ans", "de 65 à 69 ans", "70 ans et plus")], 
                           na.rm = TRUE) / sum(effectif, na.rm = TRUE),
    
    # Age moyen
    age_moyen        = sum(age_centre * effectif, na.rm = TRUE) / 
      sum(effectif[!is.na(age_centre)], na.rm = TRUE),
    
    .groups = "drop"
  )

# 4. Jointures Finales
df_final <- df_ameli %>%
  inner_join(nb_etablissements, by = "departement")

# 5. Préparation pour l'ACP
df_acp <- df_final %>%
  mutate(annee_f = as.factor(annee)) %>%
  select(
    departement,
    annee,
    annee_f,
    effectif_total_log,
    part_femmes,
    part_60_plus,
    age_moyen,
    nb_etablissements_log
  ) %>%
  na.omit()

# 6. Nettoyage de l'environnement
rm(list = setdiff(ls(), c("df_acp", "df_final")))