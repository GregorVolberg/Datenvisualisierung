library(tidyverse)
library(ggplot2)
library(patchwork)
library(readxl)

set.seed(12)

### Daten

rawN <- read_excel("C:/Users/Nathalie/Desktop/Master_Psychological_Science/1. Semester/Werkzeugkasten/3_Volberg__Datenvisualisierung/Mittelwerte_Ergebnisse_Bachelorarbeit_Nathalie_Zeus.xlsx",
                   range = "A1:D10") %>%
  rename(tmp = `...1`,
         std = `Std.-Abweichung`) %>%
  na.omit() 

indData = matrix(NA, nrow = dim(rawN)[1], ncol = rawN$N[1])
for(j in 1:dim(rawN)[1]){
  indData[j,] = rnorm(rawN$N[j],rawN$Mittelwert[j],rawN$std[j])
}

rawN2 <- bind_cols(rawN, as_tibble(indData)) %>%
  select(-Mittelwert,-std, -N) %>%
  pivot_longer(cols = V1:V31, names_to = "vp", values_to = "switchRate") %>%
  separate_wider_delim(cols = tmp, delim =  '.', names=c('context', 'sequence')) %>%
  mutate(across(context:vp, as_factor)) %>%
  select(vp, context, sequence, switchRate)


#### plotting

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

 
plotN <- ggplot(rawN2, aes(x = sequence, y = switchRate)) +
  geom_jitter(aes(color = context),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) + 
  stat_summary(
    aes(group = context),
    fun.data="mean_se",
    fun.args = list(mult = 1.5), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.6)) + 
  theme_classic() +
  scale_x_discrete(
    name = "sequence") +
  scale_y_continuous(
    name = "voluntary switching rate in %",
    limits = c(0,85)) 

## Legende ändern, Beschriftungen hinzufügen, p-Values hinzufügen

plotN + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(margin = margin(t = 0,
                                                    r = 15,
                                                    b = 0,
                                                    l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t =10,
                                                    r = 0,
                                                    b = 0,
                                                    l = 0))) +
  annotate("text", x = 0.95, y = 68,
           label = "reward",
           vjust = "center", hjust = "right",
           color = cbPalette[1]) +
  annotate("text", x = 1.05, y = 68,
           label = "loss",
           vjust = "center", hjust = "left",
           color = cbPalette[2]) + 
  annotate("segment",
           x = 1, xend = 4,
           y = 77, yend = 77) +
  annotate("segment",
           x = 1, xend = 1,
           y = 77, yend = 75) +
  annotate("segment",
           x = 2, xend = 2,
           y = 77, yend = 75) + 
  annotate("segment",
           x = 3, xend = 3,
           y = 77, yend = 75) +
  annotate("segment",
           x = 4, xend = 4,
           y = 77, yend = 75) + 
  annotate("text",
           x = 2.5,
           y = 81,
           label = "p \u2264 .001")
