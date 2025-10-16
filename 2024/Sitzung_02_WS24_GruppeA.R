# Sitzung 02, WiSe 2024, Gruppe A
library(tidyverse)

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
scale_colour_manual(values=cbPalette)

# generiere Daten, 2 x 3 - Design
df = as_tibble(data.frame(
      id    = as_factor(paste0("S",
               str_pad(1:16, 2, pad = "0"))),
      study = as_factor(rep(c("BSc", "MSc"),
                                      each=8)),
      test  = as_factor(rep(c("Anfg", "Mitte",
                     "Abschl"), each = 16)),
      score = round(c(rnorm(16, 520, 20),
                       rnorm(16, 550, 20),
                       rnorm(16, 560, 20)))))

# bar plot
set.seed(12)
g1 <- ggplot(df, 
       mapping = aes(x = test, y = score,
                     fill = study)) +
  geom_bar(aes(group = study), 
           stat = "summary",
           fun.data = "mean_se",
           fun.args = list(mult = 1), # 1 SE
           width = 0.5,
           position = position_dodge(0.6),
           alpha = 0.4) +
  scale_fill_manual(values = cbPalette) +
  geom_jitter(aes(color = study),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  geom_linerange(aes(group = study),
                 stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 size = 1) + 
  coord_cartesian(ylim = c(300,700),
                  xlim = c(0.7, 3.3)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Testwert") + 
  scale_x_discrete(labels = c("Anfang", "Mitte",
                              "Abschluss"),
                   name = "Testzeitpunkt") +
  annotate("text", x = 0.4, y = 660, 
           label = "B. Sc.",
           vjust = "center", hjust = "left",
           color = cbPalette[1]) +
  annotate("text", x = 0.4, y = 620, 
           label = "M. Sc.",
           vjust = "center", hjust = "left",
           color = cbPalette[2])

# Variante 2, mit Linien
set.seed(12)
g2 <- ggplot(df, 
       mapping = aes(x = test, y = score, fill = study)) +
  geom_jitter(aes(color = study),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  geom_line(aes(group = study, color = study),
            stat = "summary",
            fun.data = "mean_se",
            fun.args = list(mult = 1), # 1 SE
            position = position_dodge(0.6),
            size = 1) + 
  geom_pointrange(aes(group = study), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #width = 0.5,
                  position = position_dodge(0.6),
                  alpha = 0.4) +
  coord_cartesian(ylim = c(300,700),
                  xlim = c(0.7, 3.3)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Testwert") + 
  scale_x_discrete(labels = c("Anfang", "Mitte", "Abschluss"),
                   name = "Testzeitpunkt") +
  annotate("text", x = 0.4, y = 660, 
           label = "B. Sc.",
           vjust = "center", hjust = "left",
           color = cbPalette[1]) +
  annotate("text", x = 0.4, y = 620, 
           label = "M. Sc.",
           vjust = "center", hjust = "left",
           color = cbPalette[2])

# Legenden
df <- rename(df, Studienabschnitt = study)
df$Studienabschnitt <- fct_recode(df$Studienabschnitt,
                                  "B. Sc." = "BSc",
                                  "M. Sc." = "MSc")
df$Studienabschnitt <- fct_relevel(df$Studienabschnitt,
                                   "M. Sc.", "B. Sc.")
# plots arrangieren
library(patchwork)
g1 | g2

# mit Bezeichner
g1 <- g1 + labs(tag = "A")
g2 <- g2 + labs(tag = "B")
g1 | g2

# vertikal und horizontal
g1 / g2 | g2


# scatter plot, Daten
df2 <- as_tibble(data.frame(var1 = rnorm(60),
                            var2 = rnorm(60),
                 gruppe = as_factor(rep(c("A","B"), 30))))   

# for continuous x and y
plt1 <- ggplot(df2, aes(x = var1, y = var2, color = gruppe)) +
  geom_point() + 
  #geom_jitter(width = 0.05, height = 0) + 
  scale_color_manual(values = cbPalette[2:3]) +
  geom_smooth(aes(group = gruppe), method='lm', se = TRUE,
              level = 0.95, fullrange = TRUE) + # 95% CI
  theme_classic() +
  theme(legend.position="none") + 
  scale_x_continuous(expand = expansion(mult = 0.02), limits=c(-3, 3)) +
  scale_y_continuous(expand = expansion(mult = 0.02), limits=c(-3, 3))

# Randverteilungen
library(ggExtra) # contains ggMarginal
plt2 <- ggMarginal(plt1, 
                   #type = "density", 
                   type = "histogram",
                   groupColour = TRUE,
                   groupFill = TRUE)
plt2

# Einlesen von SPSS-Dateien
library(haven) # is not loaded with tidyverse
df <- read_sav("example01.sav") %>%
              pivot_longer(cols = pre:followup,
                           names_to = "test",
                           values_to = "score") %>%
              mutate(across(id:test, as_factor))

# Zweifaktorielle Messwiederholung
df <- read_sav("example02.sav") %>%
        pivot_longer(
          cols = pre_ws:pst_ss,
          names_to = c("time", "semester"),
          names_sep = "_",
          values_to = "score") %>%
        mutate(across(id:semester, as_factor))
