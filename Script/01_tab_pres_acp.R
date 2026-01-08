library(knitr)
library(kableExtra)
library(dplyr)

df_sample <- df_acp %>%
  sample_n(5) 


# Affichage du tableau
tab1 <- df_sample %>%
  kable(
    format = "latex", 
    caption = "Aperçu de 5 lignes aléatoire du jeu de données",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped", "scale_down", "HOLD_position"),
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = F,
    position = "center"
  )