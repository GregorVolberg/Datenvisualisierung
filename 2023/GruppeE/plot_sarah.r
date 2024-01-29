source('./2023/GruppeE/readSarah.r')
library(patchwork)

# color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

plt1 <- ggplot(raw, aes(x = gruppe, y = wmt)) +
  geom_jitter(width = 0.1,
              color = 'darkgrey') +
  stat_summary(aes(group = gruppe),
  fun.data="mean_se",
  fun.args = list(mult = 1.0), 
  size = 0.6,
  linewidth = 1) +
  scale_y_continuous(limits = c(0,20)) + 
  scale_color_manual(values = cbPalette) +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = 'Gruppe',
       y = 'Punkte WMT-2')

plt2 <- ggplot(raw, aes(x = gruppe, y = lgt)) +
  geom_jitter(width = 0.1,
              color = 'darkgrey') +
  stat_summary(aes(group = gruppe),
               fun.data="mean_se",
               fun.args = list(mult = 1.0), 
               size = 0.6,
               linewidth = 1) +
  scale_y_continuous(limits = c(0, 95)) + 
  scale_color_manual(values = cbPalette) +
  theme_classic() + 
  theme(legend.position = "none") +
  labs(x = 'Gruppe',
       y = 'Punkte LGT-3') +
  annotate("segment",
           x = 1, xend = 3,
           y = 90, yend = 90) +
  annotate("text",
           x = 2,
           y = 92,
           label = "p < .05",
           vjust = "bottom",
           hjust = "center") +
  annotate("segment",
           x = 2, xend = 3,
           y = 82, yend = 82) +
  annotate("text",
           x = 2.5,
           y = 84,
           label = "n. s.",
           vjust = "bottom",
           hjust = "center")

plt1 + plt2 

