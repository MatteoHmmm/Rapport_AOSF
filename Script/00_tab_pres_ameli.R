library(knitr)
library(kableExtra)
library(dplyr)

df_sample <- ameli_effectifs %>%
  sample_n(5) %>%
  # Réorganisation dans un ordre logique et limité pour affichage propre
  select(
    "Année" = annee,
    "Département" = libelle_departement,
    "Sexe" = libelle_sexe,
    "Classe d’âge" = libelle_classe_age,
    "Profession" = profession_sante
  )

# Affichage du tableau
tab1 <- df_sample %>%
  kable(
    format = "latex", 
    caption = "Aperçu de 5 lignes aléatoires du jeu de données",
    booktabs = TRUE,
    align = "cllll" # Centre l'année, aligne le reste à gauche
  ) %>%
  kable_styling(
    latex_options = c("striped", "scale_down", "HOLD_position"),
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = F,
    position = "center"
  )

rm(df_sample)