df_acm <- df_final %>%
  mutate(
    niveau_offre = cut(
      effectif_total,
      breaks = quantile(effectif_total, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Faible", "Moyen", "Élevé")
    ),
    feminisation = cut(
      part_femmes,
      breaks = quantile(part_femmes, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Faible", "Intermédiaire", "Forte")
    ),
    vieillissement = cut(
      part_60_plus,
      breaks = quantile(part_60_plus, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Jeune", "Intermédiaire", "Vieillissante")
    ),
    annee_f = as.factor(annee)
  ) %>%
  select(
    departement,
    annee,
    annee_f,
    niveau_offre,
    feminisation,
    vieillissement
  ) %>%
  na.omit()


# df_acm_2024 <- df_acm %>%
#   filter(annee == as.Date("2018-01-01"))
# 
# nrow(df_acm_2024)
# table(df_acm_2024$niveau_offre)
# table(df_acm_2024$feminisation)
# table(df_acm_2024$vieillissement)
