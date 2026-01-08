library(FactoMineR)
library(ggplot2)
library(dplyr)
library(patchwork)

df <- df_acm
df$annee_num <- format(as.Date(df$annee), '%Y')

get_acm_ind_plot <- function(target_year) {
  df_filtered <- df %>% filter(annee_num == target_year)
  df_acm_subset <- df_filtered[, c('niveau_offre', 'feminisation', 'vieillissement')]
  
  mca <- MCA(df_acm_subset, ncp = 5, graph = FALSE)
  
  coord_df <- data.frame(
    Dim1 = mca$ind$coord[, 1],
    Dim2 = mca$ind$coord[, 2],
    Departement = df_filtered$departement
  )
  
  coord_df$distance <- sqrt(coord_df$Dim1^2 + coord_df$Dim2^2)
  coord_df <- coord_df %>%
    arrange(desc(distance)) %>%
    mutate(label = ifelse(row_number() <= 15, as.character(Departement), ''))
  
  ggplot(coord_df, aes(x = Dim1, y = Dim2, label = label)) +
    geom_point(alpha = 0.6, color = '#3c8dbc', size = 1.5) +
    geom_text(data = filter(coord_df, label != ''), 
              size = 2.8, color = '#d73027', fontface = 'bold', check_overlap = TRUE) +
    labs(title = paste("Projection Individus -", target_year)) +
    theme_minimal()
}

acm_ind_compare <- get_acm_ind_plot("2010") + get_acm_ind_plot("2024")

rm(get_acm_ind_plot, df)