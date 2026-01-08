library(knitr)
library(kableExtra)
library(dplyr)

df_sample <- finess_data %>%
  sample_n(5) %>%
  # Sélection des 5 colonnes les plus pertinentes 
  select(
    "N° FINESS" = nofinesset,
    "Raison Sociale" = rs,
    "Catégorie" = libcategetab,
    "Commune" = commune,
    "Département" = libdepartement
  )

# Affichage du tableau
tab1 <- df_sample %>%
  kable(
    format = "latex", 
    caption = "Aperçu de 5 établissements (données FINESS)",
    booktabs = TRUE,
    longtable = FALSE,
    align = "l l l l l" # Alignement à gauche 
  ) %>%
  kable_styling(
    latex_options = c("striped", "scale_down", "HOLD_position"),
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  )

rm(df_sample)