# WS25, Gruppe A
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

# REBECCA
# generiere Daten, 3 x 3 - Design
df = as_tibble(data.frame(
  id      = as_factor(rep(paste0("S", str_pad(1:8, 2, pad = "0")), 9)),
  Kontext = as_factor(rep(c("Bad", "Büro", "Küche"),
                        each=8)),
  Kongruenz  = as_factor(rep(c("kongruent", "inkongruent",
                          "neutral"), each = 24)),
  RT = round(c(rnorm(24, 550, 20),
                  rnorm(24, 570, 20),
                  rnorm(24, 610, 20))),
  ER = round(c(rnorm(24, 8, 1),
               rnorm(24, 10, 1),
               rnorm(24, 11, 1)))))

# bar plot
barp <- ggplot(df, mapping = aes(x = Kontext, y = ER,
                                 fill = Kongruenz)) +
  geom_bar(aes(group = Kongruenz), 
           stat = "summary",
           width = 0.5,
           position = position_dodge(0.6),
           alpha = 0.4) +
  scale_fill_manual(values = cbPalette) +
  geom_linerange(aes(group = Kongruenz),
                 stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1) + 
  coord_cartesian(ylim = c(0,15)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Fehlerrate (%)") 

# violin plots 
viol <- ggplot(df, mapping = aes(x = Kontext, y = RT,
                        fill = Kongruenz)) +
  geom_violin(position = position_dodge(0.6),
              alpha = 0.4,
              width = 0.5,
              color = NA,
              bw = 12) +
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
