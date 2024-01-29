source('./2023/GruppeE/readNina.r')
library(patchwork)
library(ggbreak)

# color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

ggplot(vsr, aes(x = block, y = vsr)) +
  scale_color_manual(values = cbPalette) +
  coord_cartesian(ylim = c(0, 40)) + # achtung mit scale_y_continuous: stats macht nur stats über die mit den Skalenenden sichtbaren Punkte!!
  stat_summary(
    aes(group = condition),
    fun.data="mean_se",
    fun.args = list(mult = 1), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.6)) +
  stat_summary(
    aes(group = condition, color = condition),
    fun.data = "mean_se",
    geom = "line", 
    linewidth = 1,
    position = position_dodge(0.6)) +
   labs(x = "Block",
       y = "Voluntary Switch Rate (%)",
       tag = "B") +
  scale_y_break(c(0,20), scales = 2.5) +
  theme_classic() +
  theme(legend.position = "none") +
  annotate("text", x = 0.9, y = 22,
           label = 'binokular',
           vjust = 'center', hjust = 'center',
           color = cbPalette[1]) +
  annotate("text", x = 1.3, y = 20,
           label = 'monokular',
           vjust = 'center', hjust = 'center',
           color = cbPalette[2])




























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

