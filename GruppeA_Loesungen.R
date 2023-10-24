library(tidyverse)
library(patchwork)
library(ggh4x)

### plot 1: scatter plot

# surrogate data
a = rnorm(100); 
b = a + rnorm(100,0, 0.5);
b[51:100] = b[51:100]* -1
grp = c(rep('g1', 50), rep('g2', 50))
dat = tibble(a=a, b=b, grp = grp)

# simple  
ggplot(dat, mapping = aes(a, b, color = grp)) +
  geom_point() +
  stat_smooth(dat, mapping = aes(a, b, group = grp), method = lm, se = 1) +
  scale_colour_manual(values=c("red", "blue")) +
  annotate("text", x=1.5, y=2, label = 'Group 1', color = 'red', 
           vjust = "middle", hjust = 'right') +
  annotate("text", x=-1, y=2, label = 'Group 2', color = 'blue',
           vjust = "middle", hjust = 'left') + 
  scale_x_continuous(limits = c(-3, 3),
                     breaks = seq(-3,3,1),
                     minor_breaks = seq(-3,3,0.5),
                     name = "Variable A",
                     guide = "axis_minor") + 
  scale_y_continuous(limits = c(-3, 3),
                     breaks = seq(-3,3,1),
                     minor_breaks = seq(-3,3,0.5),
                     name = "Variable B", 
                     guide = "axis_minor") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.ticks.length = unit(0.2, "cm"),
    ggh4x.axis.ticks.length.minor = rel(0.5))




## ERP
ggplot() +
  geom_line(erp, 
            mapping = aes((timebin-offset)*timeBinMs, wave2),
            color = 'blue') +
  scale_x_continuous(breaks = c(-800,-400, 400, 800, 1200, 1600),
                     name = "Time (ms)") + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-0.5,0.5),
                     name = "microVolt") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.ticks.length = unit(0.2, "cm"),
    axis.text  = element_text(size=12),
    axis.title = element_text(size=12),
    panel.border = element_blank()) +
  coord_axes_inside(xintercept = offset, yintercept=0, labels_inside = TRUE) +
  guides(x = guide_axis_truncated(trunc_lower = -800,
                                  trunc_upper = 1600),
         y = guide_axis_truncated(trunc_lower = -0.5,
                                  trunc_upper = 0.5))

