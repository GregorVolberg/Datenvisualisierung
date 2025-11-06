# WS25, Gruppe B
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
set.seed(12)
ggplot(df, aes(x = intervention, 
               y = anx_score,
               fill = time,
               group = time)) +
  geom_bar(stat = "summary",
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
  geom_linerange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1) + 
  coord_cartesian(ylim = c(0,60)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Intervention", y = "Anxiety Score") + 
  annotate("segment", x = c(0.9, 1.9, 1), 
                      xend = c(1.1, 2.1, 2),
                      y = c(50, 50, 55)) +
  annotate("text", x = c(1, 2, 1.5),
                   y = c(51, 51, 56),
           label = c('p = .034',
                     'p = .041',
                     'p = .859'),
           vjust = 'bottom', hjust = 'center')  

# add: segments for individual lines
# add: legend
ggplot(df, aes(x = intervention, 
               y = anx_score,
               fill = time,
               group = time)) +
  # see geom_segment
    geom_line(
    aes(color = "gray70",
    alpha = 0.5))


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
           alpha = 0.4) ...

# JOHANNA
KG  <- rnorm(75, 25,4)
EG  <- rnorm(82, 25, 3.5)
df = as_tibble(data.frame(
     group = as_factor(c(rep("KG", length(KG)),
                         rep("EG", length(EG)))),
     self_esteem = c(KG, EG)))

library(ggdist)
# violin plots 
ggplot(df, aes(x = group,
                       y = self_esteem)) +
  stat_halfeye(
    adjust = .5, 
    width = .4, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA
  ) +
  geom_point(
    size = 2,
    alpha = .2
  ) +
  coord_cartesian(ylim = c(0,40))

https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/
  
  
scale_y_continuous()
  geom_violin(position = position_dodge(0.6),
              alpha = 0.4,
              width = 0.5,
              color = NA,
              side = 'right',
              bw = 2) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 1.5,
               position = position_dodge(0.6),
               color = NaN) +
  scale_fill_manual(values = cbPalette) +
  geom_pointrange(aes(group = Kongruenz),
                 stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 size = 0.5) + 
  coord_cartesian(ylim = c(450, 750)) + 
  scale_y_continuous(name = "Reaktionszeit (ms)") + 
  theme_classic() + 
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        #axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.15, 0.85))



library(patchwork)
viol + barp + plot_layout(heights=c(3,1))

## Moritz
# generiere Daten, (2 x 4)
# pre / post (Zeitpunkt)
# Theapieart (Delfin, Hund, Alpaka/Lama, Pferd)
# PANAS neg aff
# PANAS pos aff

# generiere Daten, 2 within x 4 between - Design, 2 AVs
df2 = as_tibble(data.frame(
  id      = as_factor(rep(paste0("S", str_pad(1:8, 2, pad = "0")), 8)),
  Therapieform  = as_factor(rep(c("Delfin", "Hund",
                                  "Alpaka", "Pferd"), 8*2)),
  Zeitpunkt = as_factor(rep(rep(c("Pre", "Post"), each = 8), 4)),
  PANAS_positiv = round(c(rnorm(64, 3, 0.5))),
  PANAS_negativ = round(c(rnorm(64, 3, 0.5)))))

pic1 <- ggplot(df2, mapping = aes(x = Zeitpunkt,
                         y = PANAS_positiv, group = Therapieform)) +
  geom_line(aes(color = Therapieform),
            stat = "summary",
            fun.data = "mean_se",
            position = position_dodge(0.2),
            size = 1) +
  geom_pointrange(aes(group = Therapieform), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  position = position_dodge(0.2),
                  alpha = 0.4) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  coord_cartesian(ylim = c(1, 5)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.8)) +
  scale_y_continuous(name = "PANAS positive affect")

pic2 <- ggplot(df2, mapping = aes(x = Zeitpunkt,
                                  y = PANAS_negativ, group = Therapieform)) +
  geom_line(aes(color = Therapieform),
            stat = "summary",
            fun.data = "mean_se",
            position = position_dodge(0.2),
            size = 1) +
  geom_pointrange(aes(group = Therapieform), 
                  stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #width = 0.5,
                  position = position_dodge(0.2),
                  alpha = 0.4) +
  scale_color_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  coord_cartesian(ylim = c(1, 5)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "PANAS negative affect")

pic1 + pic2

## Angmo
# generiere Daten
KEE <- rnorm(60, 3, 1)
SE  <- KEE + rnorm(60, 0, 0.5)   
df3 <- as_tibble(data.frame(KEE, SE))
                 
# for continuous x and y
plt1 <- ggplot(df3, aes(x = KEE, y = SE)) +
  geom_point() + 
  geom_smooth(method='lm', se = TRUE,
              level = 0.95, fullrange = TRUE) + # 95% CI
  theme_classic() +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0,5),
                  xlim = c(0,5)) 

# Randverteilungen
library(ggExtra) # contains ggMarginal
ggMarginal(plt1, 
                   #type = "density"), 
                   type = "histogram")

# Legenden
ggplot(df2, aes(x = Zeitpunkt,
                y = PANAS_positiv,
                group = Therapieform)) +
  geom_line(aes(color = Therapieform),
            stat = "summary",
            fun.data = "mean_se",
            position = position_dodge(0.2),
            size = 1) +
  theme(legend.position="none") 
# theme(legend.title = element_blank())
# theme(legend.position = c(0.1, 0.8))
