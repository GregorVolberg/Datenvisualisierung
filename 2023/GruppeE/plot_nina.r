source('./2023/GruppeE/readNina.r')
library(patchwork)

# color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

#==== vsr
ggplot(vsr, aes(x = block, y = vsr)) +
  scale_color_manual(values = cbPalette) +
  coord_cartesian(ylim = c(0, 40)) + # achtung mit scale_y_continuous: stats macht nur stats über die mit den Skalenenden sichtbaren Punkte
  stat_summary(
    aes(group = condition, color = condition),
    fun.data = "mean_se",
    geom = "line", 
    linewidth = 1,
    position = position_dodge(0.6)) +
  stat_summary(
    aes(group = condition),
    fun.data="mean_se",
    fun.args = list(mult = 1), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.6)) +
   labs(x = "Block",
       y = "Voluntary Switch Rate (%)",
       tag = "B") +
  theme_classic() +
  theme(legend.position = c(0.75,0.4),
            legend.title    = element_blank())

#===== rt
pltA1 <- ggplot(rt, aes(x = condition, y = rt)) +
  scale_color_manual(values = cbPalette[5:6]) +
  coord_cartesian(ylim = c(400, 650)) +
  stat_summary(
    aes(group = rep_swtch, color = rep_swtch),
    fun.data = "mean_se",
    geom = "line", 
    linewidth = 1,
    position = position_dodge(0.2)) +
  stat_summary(
    aes(group = rep_swtch),
    fun.data="mean_se",
    fun.args = list(mult = 1), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.2)) +
  theme_classic() +
  theme(legend.position = c(0.25,0.2),
        legend.title    = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line.x=element_blank()) +
  labs(y = "Reaction time (ms)",
       tag = "A")

pltA2 <- ggplot(rt, aes(x = condition, y = error)) +
  scale_fill_manual(values = cbPalette[5:6]) +
  coord_cartesian(ylim = c(0, 15)) +
  stat_summary(
    aes(group = rep_swtch, fill = rep_swtch),
    fun.data = "mean_se",
    geom = "bar", 
    width = 0.2,
    position = position_dodge2(0.2))  +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Errors (%)",
       x = "Condition")

pltA1 + pltA2 + plot_layout(heights = c(2,1))
