# WS25, Gruppe B
library(tidyverse)

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# To use for fills, add
# scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

# JANINA
# generate Data, 2 x 2 - Design
n <- 20*2
pre  <- rnorm(n, 40,5)
post <- rnorm(n, 30,5)
df = as_tibble(data.frame(
  id      = as_factor(rep(paste0("S", str_pad(1:n, 2, pad = "0")), 2)),
  time    = as_factor(rep(c("pre", "post"), each = n)),
  intervention = as_factor(rep(rep(c("Body Scan", "Mandala"), each=n/2),2)),
  anx_score = c(pre, post)))

# bar plot
ggplot(df, aes(x = intervention, 
               y = anx_score,
               fill = time,
               group = time)) +
  geom_bar(stat = "summary",
           width = 0.5,
           position = position_dodge(0.6),
           alpha = 0.4) +
  geom_jitter(aes(color = time),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
  geom_linerange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1) + 
  coord_cartesian(ylim = c(0,65)) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.7),
       legend.title = element_blank()) +
  labs(x = "Intervention", y = "Anxiety Score") + 
  annotate("segment", x = c(0.9, 1.9, 1), 
                      xend = c(1.1, 2.1, 2),
                      y = c(55, 55, 60)) +
  annotate("text", x = c(1, 2, 1.5),
                   y = c(56, 56, 61),
           label = c('p = .034',
                     'p = .041',
                     'p = .859'),
           vjust = 'bottom', hjust = 'center')  


# ANTONIA
n <- 40
A  <- rnorm(n/2, 0.6,0.1)
B <- rnorm(n/2, 0.8, 0.1)
df = as_tibble(data.frame(
  id       = as_factor(paste0("S", str_pad(1:n*2, 2, pad = "0"))),
  group    = as_factor(rep(rep(c("Individualgruppe", 
                                 "Kontrollgruppe"), each=n/2),2)),
  accuracy = c(A, B)))

ggplot(df, aes(x = group, 
               y = accuracy)) +
  geom_bar(stat = "summary",
           width = 0.5,
           position = position_dodge(0.6),
           alpha = 0.4) ... # see above

# JOHANNA
set.seed(12)
KG  <- rnorm(75, 25,4)
EG  <- rnorm(82, 25, 3.5)
df = as_tibble(data.frame(
     group = as_factor(c(rep("control", length(KG)),
                         rep("experimental", length(EG)))),
     self_esteem = c(KG, EG)))

library(ggdist) # for violin plots 

ggplot(df, aes(x = group,
               y = self_esteem)) +
  stat_halfeye(
    adjust = .5, 
    width = .4, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) +
  geom_jitter(size = 2,
              width = 0.02,
              color = 'gray80') +
  geom_pointrange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1.96), # i.e, 95%CI
                 linewidth = 1) +
  coord_cartesian(ylim = c(0,40)) +
  labs(x = "Group", y = "Self Esteem Score") + 
  theme_classic() 