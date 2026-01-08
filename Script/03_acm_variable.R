library(FactoMineR)
library(factoextra)
library(dplyr)
library(patchwork)


df <- df_acm
df$annee_num <- format(as.Date(df$annee), '%Y')

# --- Fonction pour générer le graphique ACM par année ---
get_mca_plot <- function(target_year) {
  df_filtered <- df %>% filter(annee_num == target_year)
  df_acm <- df_filtered[, c('niveau_offre', 'feminisation', 'vieillissement')]
  
  mca <- MCA(df_acm, ncp = 5, graph = FALSE)
  
  fviz_mca_var(mca,
               axes = c(1, 2),
               col.var = 'contrib',
               gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
               repel = TRUE,
               labelsize = 3.5, # Taille réduite pour la lisibilité côte à côte
               ggtheme = theme_minimal()) +
    labs(title = paste("ACM Variables -", target_year))
}

# Création des deux graphiques
g1 <- get_mca_plot("2010")
g2 <- get_mca_plot("2024")

# Assemblage final
comparaison_acm <- g1 + g2

rm(g1, g2, get_mca_plot, df)