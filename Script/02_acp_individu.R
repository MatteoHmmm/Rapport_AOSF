library(FactoMineR)
library(ggplot2)
library(dplyr)
library(patchwork)

df <- df_acp
df$annee_num <- format(as.Date(df$annee), '%Y')

# --- Fonction pour générer le graphique par année ---
generer_projection <- function(annee_cible) {
  df_filtered <- df %>% filter(annee_num == annee_cible)
  
  # Sélection des variables et suppression des NA
  df_acp_raw <- df_filtered %>% 
    select(effectif_total_log, part_femmes, part_60_plus, age_moyen, nb_etablissements_log)
  
  indices_valides <- complete.cases(df_acp_raw)
  df_acp <- df_acp_raw[indices_valides, ]
  depts_valides <- df_filtered$departement[indices_valides]
  
  pca <- PCA(df_acp, scale.unit = TRUE, ncp = 5, graph = FALSE)
  
  coord_df <- data.frame(
    Dim1 = pca$ind$coord[, 1],
    Dim2 = pca$ind$coord[, 2],
    Departement = depts_valides
  )
  
  coord_df$distance <- sqrt(coord_df$Dim1^2 + coord_df$Dim2^2)
  
  # Labels pour les 15 points les plus éloignés
  coord_df <- coord_df %>%
    arrange(desc(distance)) %>%
    mutate(label = ifelse(row_number() <= 15, as.character(Departement), ''))
  
  ggplot(coord_df, aes(x = Dim1, y = Dim2, label = label)) +
    geom_point(alpha = 0.5, color = '#3c8dbc', size = 1.5) +
    geom_text(data = filter(coord_df, label != ''), 
              size = 2.5, color = '#d73027', fontface = 'bold', check_overlap = TRUE) +
    labs(title = paste("Nuage des individus", annee_cible), x = "Dim 1", y = "Dim 2") +
    theme_minimal()
}

# Génération des deux plots
p_2010 <- generer_projection("2010")
p_2024 <- generer_projection("2024")

# Combinaison
projection_comparee <- p_2010 + p_2024

rm(df, p_2010, p_2024, generer_projection)