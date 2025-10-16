# Sitzung 02, WiSe 2024, Gruppe B
library(tidyverse)

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# # To use for fills, add
# scale_fill_manual(values=cbPalette)
# # To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

# generiere Daten, 2 x 6 - Design
df <- as_tibble(data.frame(
      id      = as_factor(paste0("S",
               str_pad(1:8, 2, pad = "0"))),
      zeit    = as_factor(rep(paste0("t", 1:6),
                                   each=8)),
      hund    = as_factor(rep(c("mit Hund", "ohne Hund"),
                             each = 48)),
      stress  = rnorm(48*2, 3.5, 0.5),
      stress2 = rnorm(48*2, 3.5, 0.5)))

# line plot
set.seed(12)
ggplot(df, mapping = aes(x = zeit,
            y = stress, group = hund)) +
  geom_line(aes(color = hund),
            stat = "summary",
            fun.data = "mean_se",
            position = position_dodge(0.6),
            size = 1) +
  scale_color_manual(values = cbPalette[2:3]) +
  geom_jitter(aes(color = hund),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  geom_pointrange(aes(group = hund), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #width = 0.5,
                  position = position_dodge(0.6),
                  alpha = 0.4) +
  coord_cartesian(xlim = c(1, 6),
                  ylim = c(2, 5)) +
  theme_classic() +
  #theme(legend.position = "none") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.9)) +
  scale_y_continuous(name = "AA Stress Scale") + 
  scale_x_discrete(labels = c("t1", "pre", "post",
                              "t4", "t5", "t6"),
                   name = "Testzeitpunkt") +
  annotate("rect", xmin = 1.7, xmax = 3.3,
           ymin = 2.1, ymax = 4.9,
           alpha = .2,fill = "gray")


# Legenden
df2 <- df
df2 <- rename(df2, Bedingung = hund)
df2$Bedingung <- fct_recode(df2$Bedingung,
                                  "Hund" = "mit Hund",
                                  "kein Hund" = "ohne Hund")
df2$Bedingung <- fct_relevel(df2$Bedingung,
                                   "kein Hund", "Hund")

# mehrere plots
plt_stress1 <- ggplot(df, 
                      mapping = aes(x = zeit, y = stress, group = hund)) +
  geom_jitter(aes(color = hund),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette[2:3]) +
  geom_line(aes(color = hund),
            stat = "summary",
            fun.data = "mean_se",
            fun.args = list(mult = 1), # 1 SE
            position = position_dodge(0.6),
            size = 1) + 
  geom_pointrange(aes(group = hund), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #width = 0.5,
                  position = position_dodge(0.6),
                  alpha = 0.4) +
  coord_cartesian(xlim = c(1, 6),
                  ylim = c(2, 5)) +
  theme_classic() +
  #theme(legend.position = "none") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.9),
        axis.title.x = element_blank()) +
  scale_y_continuous(name = "AA Stress Scale") + 
  scale_x_discrete(labels = c("t1", "pre", "post",
                              "t4", "t5", "t6")) +
  annotate("rect", xmin = 1.7, xmax = 3.3,
           ymin = 2.1, ymax = 4.9,
           alpha = .2,fill = "gray")

plt_stress2 <- ggplot(df, 
                      mapping = aes(x = zeit, y = stress2, group = hund)) +
  geom_jitter(aes(color = hund),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette[2:3]) +
  geom_line(aes(color = hund),
            stat = "summary",
            fun.data = "mean_se",
            fun.args = list(mult = 1), # 1 SE
            position = position_dodge(0.6),
            size = 1) + 
  geom_pointrange(aes(group = hund), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #width = 0.5,
                  position = position_dodge(0.6),
                  alpha = 0.4) +
  coord_cartesian(xlim = c(1, 6),
                  ylim = c(2, 5)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Stress Scale 2") + 
  scale_x_discrete(labels = c("t1", "pre", "post",
                              "t4", "t5", "t6"),
                   name = "Testzeitpunkt") +
  annotate("rect", xmin = 1.7, xmax = 3.3,
           ymin = 2.1, ymax = 4.9,
           alpha = .2,fill = "gray")

library(patchwork)
p1 <- ggplot(mpg, aes(x = hwy)) +
        geom_histogram()
p2 <- ggplot(mpg, aes(x = hwy,
                      y = cty)) +
  geom_point()

# vertikal
p1 / p2

# horizontal
p1 | p2

# vertikal und horizontal
(p1 | p2) / (p1)

# mit Bezeichner
p1 <- p1 + labs(tag = "A")
p2 <- p2 + labs(tag = "B")
p1 / p2

# vorliegende Daten
# vertikal
plt_stress1 / plt_stress2

# horizontal
plt_stress1 | plt_stress2

# vertikal und horizontal
(plt_stress2 | plt_stress1) / (plt_stress1)


##########################
# generiere Daten, 2 x 2 - Design
dfp <- as_tibble(data.frame(
  id      = as_factor(paste0("S",
                      str_pad(1:8, 2, pad = "0"))),
  Gestalten = as_factor(rep(c("ohne", "mit"),
                          each=8)),
  Kommunikation    = as_factor(rep(c("neutral", "Beitragszweck"),
                          each = 16)),
  mfw     = rnorm(16*2, 3.5, 0.5)))

# wie zuvor...
set.seed(12)
ggplot(dfp, mapping = aes(x = Kommunikation,
                         y = mfw, group = Gestalten)) +
  geom_jitter(aes(color = Gestalten),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  geom_line(aes(color = Gestalten),
            stat = "summary",
            fun.data = "mean_se",
            fun.args = list(mult = 1), # 1 SE
            position = position_dodge(0.6),
            size = 1) + 
  geom_pointrange(aes(group = Gestalten), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #width = 0.5,
                  position = position_dodge(0.6),
                  alpha = 0.4) +
  coord_cartesian(xlim = c(1, 2),
                  ylim = c(2, 5)) +
  theme_classic() +
  #theme(legend.position = "none") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.9)) +
  scale_y_continuous(name = "MFW")


##########################
# generiere Daten, 2 x 2 - Design
dfa <- as_tibble(data.frame(
  id      = as_factor(paste0("S",
                             str_pad(1:8, 2, pad = "0"))),
  Zeit = as_factor(rep(c("pre", "post"),
                            each=8)),
  Bedingung    = as_factor(rep(c("Text Empathie", "Text Nutzung"),
                                   each = 16)),
  Empathieskala     = rnorm(16*2, 3.5, 0.5)))

# wie zuvor...
set.seed(12)
ggplot(dfa, mapping = aes(x = Zeit,
                          y = Empathieskala, group = Bedingung)) +
  geom_jitter(aes(color = Bedingung),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  geom_line(aes(color = Bedingung),
            stat = "summary",
            fun.data = "mean_se",
            fun.args = list(mult = 1), # 1 SE
            position = position_dodge(0.6),
            size = 1) + 
  geom_pointrange(aes(group = Bedingung), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #width = 0.5,
                  position = position_dodge(0.6),
                  alpha = 0.4) +
  coord_cartesian(xlim = c(1, 2),
                  ylim = c(2, 5)) +
  theme_classic() +
  #theme(legend.position = "none") +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.9))



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