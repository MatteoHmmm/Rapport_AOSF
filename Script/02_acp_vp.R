library(FactoMineR)
library(factoextra)
library(dplyr)
library(patchwork)

df <- df_acp

# --- Fonction pour générer l'éboulis ---
get_scree_plot <- function(target_year, bar_color) {
  # Filtrage des données pour l'année spécifique
  data_year <- df %>% 
    filter(format(as.Date(annee), '%Y') == target_year) %>%
    select(effectif_total_log, part_femmes, part_60_plus, age_moyen, nb_etablissements_log) %>%
    na.omit()
  
  # Calcul de l'ACP
  pca_obj <- PCA(data_year, scale.unit = TRUE, ncp = 5, graph = FALSE)
  
  # Génération du Scree Plot
  fviz_eig(pca_obj, 
           addlabels = TRUE, 
           ylim = c(0, 60), # Fixer l'axe Y pour faciliter la comparaison
           barfill = bar_color, 
           barcolor = "black",
           main = paste("Éboulis des valeurs propres (", target_year, ")", sep="")) +
    theme_minimal()
}

# Génération des deux graphiques
e1 <- get_scree_plot("2010", "#999999") # Gris pour 2010
e2 <- get_scree_plot("2024", "#3c8dbc") # Bleu pour 2024

# Assemblage côte à côte
comparaison_eboulis <- e1 + e2
rm(e1, e2, df, get_scree_plot)