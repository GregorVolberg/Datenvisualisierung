# Sitzung 02, WiSe 2024, Gruppe A

library(tidyverse)

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")


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
  ylab("Testwert") +
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
                  width = 0.5,
                  position = position_dodge(0.6),
                  alpha = 0.4) +
  coord_cartesian(ylim = c(300,700),
                  xlim = c(0.7, 3.3)) +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("Testwert") +
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

# 2 x 2 x 2 plot
library(patchwork)
# zwei separate plot produzieren, dann nebeneinander platzieren
g1 | g2



library(ggExtra) # contains ggMarginal
n = 60
df3 <- as_tibble(data.frame(var1 = rnorm(n),
                            var2 = rnorm(n),
                 gruppe = as_factor(rep(c("A","B"), n/2))))   

# df3 <- as_tibble(data.frame(var1 = sample(1:5,n, replace =T),
#                             var2 = rnorm(60),
#                             gruppe = as_factor(rep(c("A","B"),30))))

# for continuous x and y, use geom_point
plt1 <- ggplot(df3, aes(x = var1, y = var2, color = gruppe)) +
  geom_point() + # use for continuous x and y
  #geom_jitter(width = 0.05, height = 0) + #add some jitter to avoid overplotting
  scale_color_manual(values = cbPalette[2:3]) +
  geom_smooth(aes(group = gruppe), method='lm', se = TRUE, level = 0.95,
              fullrange = TRUE) + # 95% confidence interval
  theme_classic() +
  theme(legend.position="none") + 
  scale_x_continuous(expand = expansion(mult = 0.02), limits=c(-3, 3)) +
  scale_y_continuous(expand = expansion(mult = 0.02), limits=c(-3, 3))


plt2 <- ggMarginal(plt1, 
                   #type = "density", 
                   type = "histogram",
                   groupColour = TRUE,
                   groupFill = TRUE)
plt2


# library(haven) # is not loaded with tidyverse
# 
# id      <- paste0("id",str_pad(1:16, 2, pad = "0"))
# study   <- sample(c("B. Sc.", "M. Sc."), 16, replace = TRUE)
# test1   <- round(rnorm(16, 520,20))
# test2   <- round(test1 + rnorm(16, 30,10))
# test3   <- round(test2 + rnorm(16, 10,3))
# df      <- as_tibble(data.frame(id, study, test1, test2, test3)) %>%
#               pivot_longer(cols = test1:test3,
#                            names_to = "test",
#                            values_to = "score") %>%
#               mutate(across(id:test, as_factor))
# 
# 
# 
# id      <- paste0("id",str_pad(1:16, 2, pad = "0"))
# stud    <- as_factor(rep(c("B. Sc.", "M. Sc."), 8))
# pre_ws  <- round(rnorm(16, 520,20))
# pst_ws  <- round(pre_ws + rnorm(16, 30,10))
# pre_ss  <- round(pre_ws + rnorm(16, 10,3))
# pst_ss  <- round(pst_ws - rnorm(16, 20,5))
# df      <- as_tibble(data.frame(id, study, pre_ws, pst_ws, pre_ss, pst_ss))
# 
# # ## Einlesen und modifizieren der Daten
# # df1 <- read_sav("example01.sav") %>%
# #         pivot_longer(
# #           cols = pre_ws:pst_ss,
# #           names_to = c("time", "semester"),
# #           names_sep = "_",
# #           values_to = "score") %>%       
# #         mutate(across(id:semester, as_factor),
# #                study = fct_recode(study,
# #                           Master = "MSc",
# #                           Bachelor = "BSc"),
# #                study = fct_relevel(study,
# #                                    "Bachelor",
# #                                    "Master"))
# 
# df1 <- read_sav("exampleL07.sav") %>%
#   pivot_longer(cols = c(pre, post1, post2),
#                names_to  = "time",
#                values_to = "RT") %>%
#   mutate(time = as_factor(time),
#          time = fct_recode(time,
#                            vorher = "pre",
#                            nachher = "post1",
#                            followup = "post2"),
#          group = as_factor(group),
#          group = fct_recode(group,
#                             Kontrolle = "cont",
#                             Treatment = "exp"),
#          group = fct_relevel(group,
#                              "Treatment",
#                              "Kontrolle")) # Reihenfolge
