library(tidyverse)
library(ggplot2)
library(patchwork)


dframe <- tibble(
  av = rnorm(100, 10, 1),
  uvA = as.factor(rep(c("A1", "A2"), each = 50)),
  uvB = as.factor(rep(rep(c("B1", "B2"), each = 25), 2)))


plt <- ggplot(dframe, aes(x = uvA, y = av)) +
  geom_jitter(aes(color = uvB),
              size = 2,
              position = position_jitterdodge(
                         jitter.width = 0.2,
                         dodge.width  = 0.6)) +
  
  stat_summary(
    aes(group = uvB),
    fun.data="mean_se",
    fun.args = list(mult = 1.5), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.6)) + 
  
  scale_x_discrete(labels = c("Level 1", "Level2"),
                   name   = "Faktor A") + 
  scale_y_continuous(limits = c(6, 15),
                     breaks = seq(6,12,1),
                     name = "Meine Variable")


# use color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

myPlot <- plt +
  theme_classic() +
  scale_color_manual(values = cbPalette) +
  theme(legend.position = "none") +
  annotate("text", x = 1.9, y = 7,
           label = 'Level 1',
           vjust = "center", hjust = "right",
           color = cbPalette[1]) +
  annotate("text", x = 2.1, y = 7,
         label = 'Level 2',
         vjust = "center", hjust = "left",
         color = cbPalette[2]) +
  annotate("segment",
           x = 0.8, xend = 1.2,
           y = 13, yend = 13) +
  annotate("text",
           x = 1,
           y = 13.3,
           label = "p = .034") +
  annotate("segment",
           x = 1.8, xend = 2.2,
           y = 13, yend = 13) +
  annotate("text",
           x = 2,
           y = 13.3,
           label = "n. s.") +
  annotate("segment",
         x = 1, xend = 2,
         y = 14, yend = 14) +
  annotate("text",
         x = 1.5,
         y = 14.3,
         label = "n. s.")

# arrangieren
plot1 <- myPlot + labs(tag = "A")
plot2 <- myPlot + labs(tag = "B")

plot1 | plot2
plot1 | plot2 / plot1


# faceting
dframe2 <- tibble(
  av = rnorm(200, 10, 1),
  uvA = as.factor(rep(c("A1", "A2"), each = 100)),
  uvB = as.factor(rep(rep(c("B1", "B2"), each = 50), 2)),
  uvC = as.factor(rep(rep(c("C1", "C2"), each = 25), 4)))

ggplot(dframe2, aes(x = uvA, y = av)) +
  geom_jitter(aes(color = uvB),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  stat_summary(
    aes(group = uvB),
    fun.data="mean_se",
    fun.args = list(mult = 1.5), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.6)) +
  facet_wrap(~uvC, strip.position = "bottom") +
  theme_classic() + 
  theme(strip.background = element_blank())

# labelling siehe 
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2

