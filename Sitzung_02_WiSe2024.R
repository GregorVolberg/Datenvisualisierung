# Sitzung 02, WiSe 2024

library(tidyverse)
library(haven) # is not loaded with tidyverse

## Einlesen und modifizieren der Daten
df1 <- read_sav("exampleL07.sav")
df2 <- pivot_longer(df1,
                    cols = c(pre, post1, post2),
                    names_to  = "time",
                    values_to = "RT") %>%
        mutate(time = as_factor(time),
               time = fct_recode(time, 
                                 vorher = "pre", 
                                 nachher = "post1",
                                 followup = "post2"),
               group = as_factor(group),
               group = fct_recode(group,
                                  Kontrolle = "cont",
                                  Treatment = "exp"),
               group = fct_relevel(group,
                                   "Treatment",
                                   "Kontrolle")) # Reihenfolge

# Farben
# use color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")



# Bar- oder Strip-Plot
ggplot(df2, 
       mapping = aes(x = time, y = RT, fill = group)) +
  stat_summary(aes(group = group), 
               position = position_dodge(0.6),
               geom = "bar",
               width = 0.5,
               alpha = 0.4) + 
  scale_fill_manual(values = cbPalette) +
  geom_jitter(aes(color = group),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  stat_summary(aes(group = group), 
               position = position_dodge(0.6),
               geom = "errorbar",
               width = 0) + 
  theme_classic() +

  coord_cartesian(ylim = c(200,700),
                  xlim = c(0.7, 3.3)) +
  ylab("Reaktionszeit (ms)") +
  scale_x_discrete(labels = c("vorher", "nachher", "follow-up"),
                   name = "Zeitpunkt") 
  

    

# Achsen limits entweder mit  coord_cartesian (besser)
coord_cartesian(ylim = c(0,700))

# oder mehr konfektinierung möglich mit scale_x_discrete etc (geht nicht mit stat_summary)
scale_x_discrete(labels = c("vorher", "nachher", "follow-up"),
                 name = "Zeitpunkt") +
scale_y_continous(limits = c(200, 700),
                  breaks = seq(200, 700, 50),
                  name = "Reaktionszeit (ms)")

# achsen- Labels mit xlab, ylab
xlab("Erhebungszeitpunkt") + 
  ylab("Reaktionszeit (ms)")

# kategoriale Achse mit scale_x_discrete
scale_x_discrete(labels=c("post1" = "Dose 0.5", "1" = "Dose 1",
                          "2" = "Dose 2"))

# color map ändern mit 
scale_color_manual() für aussenlinie
scale_fill_manual() für innen

# error bar mit stat_summary und geom = "errorbar"

  ggplot(df2, aes(x = time, y = RT, 
                fill = group)) + 
  geom_col(position = "dodge", stat = "summary", fun.y = "mean")
  
# abstand zwischen 2*2 plots entweder mit Paket patchwork (2 plots nebeneinander)
# oder 
  
  