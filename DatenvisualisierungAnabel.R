
#######################################
### Datenvisualisierung ###
#############################
# Überarbeitung Daigram Bachelorarbeit #
#######################################

library(tidyverse)
#install.packages("haven")
library(haven)



data_anabel <- function(){
  read_spss("./Datensatz_Umfrage_Bachelorarbeit.sav") %>%
    mutate(
      Haustier = as_factor(Haustier_zusammen),
      lfdn     = as_factor(lfdn)
    ) %>%
    select(
      lfdn,
      Haustier,
      Depression_DASS,
      Angst_DASS,
      Stress_DASS
    ) %>%
    filter(Haustier %in% c("Haustier", "KeinHaustier"))
}


df <- data_anabel()

glimpse(df)
count(df, Haustier)


plot_dass <- function(df, dv, ylab){
  
  ggplot(df, aes(x = Haustier, y = .data[[dv]], fill = Haustier)) +
    
    geom_bar(
      stat = "summary",
      fun.data = "mean_se",
      width = 0.6,
      alpha = 0.5
    ) +
    
    geom_linerange(
      stat = "summary",
      fun.data = "mean_se",
      fun.args = list(mult = 1),   # 1 SE
      linewidth = 0.9,
      color = "black"
    ) +
    
    geom_jitter(
      aes(color = Haustier),
      width = 0.12,
      size = 1.6,
      alpha = 0.6
    ) +
    
    scale_fill_manual(values = cbPalette[c(1,2)]) +
    scale_color_manual(values = cbPalette[c(1,2)]) +
    
    theme_classic() +       
    theme(legend.position = "none") +
    
    labs(
      x = "Haustierbesitz",
      y = ylab
    ) +
    
    coord_cartesian(ylim = c(0, NA))
}



plot_dass(df, "Depression_DASS", "DASS Depression (M ± 1 SE)")
plot_dass(df, "Angst_DASS",      "DASS Angst (M ± 1 SE)")
plot_dass(df, "Stress_DASS",     "DASS Stress (M ± 1 SE)")


df_long <- df %>%
  pivot_longer(
    cols = c(Depression_DASS, Angst_DASS, Stress_DASS),
    names_to = "Skala",
    values_to = "Wert"
  ) %>%
  mutate(
    Skala = factor(Skala, levels = c("Angst_DASS", "Depression_DASS", "Stress_DASS")),
    Haustier = factor(Haustier, levels = c("Haustier", "KeinHaustier"))
  )

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")




cutoffs <- tibble(
  Skala = c("Depression_DASS", "Angst_DASS", "Stress_DASS"),
  cutoff = c(10, 8, 10)
)



ggplot(df_long, aes(x = Haustier, y = Wert)) +
  

  geom_bar(
    aes(fill = Skala, alpha = Haustier),
    stat = "summary",
    fun.data = "mean_se",
    width = 0.6,
    position = position_dodge(width = 0.65)
  ) +
  
  
  geom_linerange(
    aes(group = Haustier),
    stat = "summary",
    fun.data = "mean_se",
    fun.args = list(mult = 1),
    linewidth = 0.9,
    color = "black",
    position = position_dodge(width = 0.65)
  ) +
  
  
  geom_hline(
    data = cutoffs,
    aes(yintercept = cutoff),
    linetype = "dashed",
    color = "red"
  ) +
 
  

  geom_jitter(
    aes(color = Skala, alpha = Haustier),
    width = 0.12, height = 0,
    size = 1.4
  ) +
  scale_color_manual(values = skala_cols) +

  
  facet_wrap(~ Skala, nrow = 1) +
  
  scale_fill_manual(values = skala_cols) +
  scale_alpha_manual(values = c(Haustier = 0.55, KeinHaustier = 0.90)) +
  
  theme_classic() +
  theme(
    strip.text = element_blank(),        
    strip.background = element_blank()
  ) +
  
  labs(x = "Haustierbesitz", y = "DASS-Wert (M ± 1 SE)") +
  
  coord_cartesian(ylim = c(0, 22)) +
  
  guides(alpha = "none", fill = "none") 






