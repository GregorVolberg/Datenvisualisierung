library(tidyverse)
library(foreign)
library(patchwork)

setwd("C:/Users/Sarah/OneDrive - Servereinhorn/Dokumente/UNI/Master Semester 1/Werkzeugkasten Research Skills")
filename <- "Daten.sav"

df <- read.spss(filename) %>%
  as_tibble() %>%
  mutate(vp = as_factor(VP_Nummer)) %>%
  select(vp, alter = Alter, geschlecht = Geschlecht, 
         gruppe = GR01, wmt = WM_Gesamt, lgt = LGT3)


df <- df %>%
  mutate(wmt_percent = (wmt / 18) * 100,
         lgt_percent = (lgt / 80) * 100)

# Personalisierte Farben für jede Gruppe
gruppen_farben <- c("Tisch" = "darkblue", "Tasche" = "#3498db", "Forest" = "#999999")

# Plot
Plot_wmt <- ggplot(df, aes(x = gruppe, y = wmt_percent, color = gruppe)) +
  geom_jitter(width = 0.05) + 
  stat_summary(geom = 'pointrange', fun.data = 'mean_se') + 
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Gruppe",
       y = "Richtig gelöste Items in %",
       # tag = "WMT"
       ) +
  scale_color_manual(values = gruppen_farben) +
  theme_classic() +
  theme(legend.position = "none") + 
  ggtitle("Wiener Matrizen Test") + 
  theme(plot.title = element_text(hjust = 0.5))

print(Plot_wmt)


#### LGT 
Plot_lgt <- ggplot(df, aes(x = gruppe, y = lgt_percent, color = gruppe)) +
  geom_jitter(width = 0.05) + 
  stat_summary(geom = 'pointrange', fun.data = 'mean_se') + 
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Gruppe",
       y = "Richtig gelöste Items in %",
      # tag = "LGT"
       ) +
  scale_color_manual(values = gruppen_farben) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_segment(aes(x = "Tisch", xend = "Tasche", y = 78, yend = 78), linetype = "solid", color = "black") +
  geom_segment(aes(x = "Tisch", xend = "Tisch", y = 76, yend = 78), linetype = "solid", color = "black") +
  geom_segment(aes(x = "Tasche", xend = "Tasche", y = 76, yend = 78), linetype = "solid", color = "black") + 
  geom_text(aes(x=1.5, y = 80), label ="signifikant", color ="black") +
  #geom_text(aes(x = (which(levels(df$gruppe) == "Tisch") + which(levels(df$gruppe) == "Tasche")) / 3.5, y = 80, label = "signifikant"), color = "black", size = 3) + 
  ggtitle("Lern und Gedächtnistest") + 
  theme(plot.title = element_text(hjust = 0.5))
  
print(Plot_lgt)


Plot_lgt | Plot_wmt                                                                                                                                           

