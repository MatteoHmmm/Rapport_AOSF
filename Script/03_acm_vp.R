library(FactoMineR)
library(factoextra)
library(dplyr)
library(patchwork)

df <- df_acm
df$annee_num <- format(as.Date(df$annee), '%Y')

# --- Fonction pour générer l'éboulis ACM ---
get_acm_scree_plot <- function(target_year, bar_color) {
  # Filtrage et sélection des variables qualitatives
  data_year <- df %>% 
    filter(annee_num == target_year) %>%
    select(niveau_offre, feminisation, vieillissement)
  
  # Calcul de l'ACM
  mca_obj <- MCA(data_year, ncp = 5, graph = FALSE)
  
  # Génération du Scree Plot
  fviz_eig(mca_obj, 
           addlabels = TRUE, 
           ylim = c(0, 50), 
           barfill = bar_color, 
           barcolor = "black",
           main = paste("Éboulis ACM (", target_year, ")", sep="")) +
    theme_minimal()
}

# Génération des deux graphiques
e1_acm <- get_acm_scree_plot("2010", "#95a5a6") # Gris
e2_acm <- get_acm_scree_plot("2024", "#3c8dbc") # Bleu

# Assemblage côte à côte
acm_eboulis_compare <- e1_acm + e2_acm

rm(e1_acm, e2_acm, get_acm_scree_plot, df)