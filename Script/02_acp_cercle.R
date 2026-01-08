library(FactoMineR)
library(factoextra)
library(dplyr)
library(patchwork) 

df <- df_acp
df$annee_num <- format(as.Date(df$annee), '%Y')

# --- Graphique 2010 ---
df_2010 <- df %>% filter(annee_num == '2010') %>%
  select(effectif_total_log, part_femmes, part_60_plus, age_moyen, nb_etablissements_log) %>%
  na.omit()

pca_2010 <- PCA(df_2010, scale.unit = TRUE, ncp = 5, graph = FALSE)
p1 <- fviz_pca_var(pca_2010, col.var = 'contrib', title = "Cercle 2010", repel = TRUE)

# --- Graphique 2024 ---
df_2024 <- df %>% filter(annee_num == '2024') %>%
  select(effectif_total_log, part_femmes, part_60_plus, age_moyen, nb_etablissements_log) %>%
  na.omit()

pca_2024 <- PCA(df_2024, scale.unit = TRUE, ncp = 5, graph = FALSE)
p2 <- fviz_pca_var(pca_2024, col.var = 'contrib', title = "ACP 2024", repel = TRUE)

# Créer l'objet combiné
graphique_final <- p1 + p2

rm(df_2010, df_2024, pca_2010, pca_2024, p1, p2, df)