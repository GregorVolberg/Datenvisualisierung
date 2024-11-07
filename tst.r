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

# generiere Daten, 2 x 2 x 2 x 2 - Design
df = as_tibble(data.frame(
  id    = as_factor(paste0("S",
                           str_pad(1:8, 2, pad = "0"))),
  training     = as_factor(rep(c("no", "yes"),
                               each=8)),
  orientation  = as_factor(rep(c("radial", "tangential"),
                               each = 16)),
  vfield       = as_factor(rep(c("LVF", "RVF"),
                               each = 32)),
  time         = as_factor(rep(c("pre", "post"),
                               each = 64)),
  score        = rnorm(8*2*2*2, 60,10))) %>%
  arrange(id, training, orientation, vfield, time)

# bar plot
set.seed(12)
ggplot(df, 
             mapping = aes(x = training, y = score,
                           fill = time)) +
  facet_grid(orientation ~ vfield,
             switch = 'x') +
  geom_bar(aes(group = time), 
           stat = "summary",
           fun.data = "mean_se",
           fun.args = list(mult = 1), # 1 SE
           width = 0.5,
           position = position_dodge(0.6),
           alpha = 0.4) +
  scale_fill_manual(values = cbPalette) +
  geom_jitter(aes(color = time),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  geom_linerange(aes(group = time),
                 stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 size = 1) + 
  coord_cartesian(ylim = c(0,100),
                  xlim = c(0.7, 2.3)) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank()) +
  scale_y_continuous(name = "Hitrate (%)") + 
  scale_x_discrete(labels = c("Kontrolle", "Training"),
                   name = "Gruppe") +
  annotate("text", x = 0.2, y = 95, 
           label = "vorher",
           vjust = "left", hjust = "left",
           color = cbPalette[1]) +
  annotate("text", x = 0.2, y = 88, 
           label = "nachher",
           vjust = "left", hjust = "left",
           color = cbPalette[2])