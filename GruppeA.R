library(tidyverse)
library(patchwork)
library(ggh4x)

### scatter plot mit lm
# surrogate data
a = rnorm(100); 
b = a + rnorm(100,0, 0.5);
b[51:100] = b[51:100]* -1
grp = c(rep('g1', 50), rep('g2', 50))
dat = tibble(a=a, b=b, grp = grp)

# einfach  
ggplot(dat, mapping = aes(a, b)) +
  geom_point() +
  stat_smooth(method = lm, se = 1)
  
# zwei Gruppen
ggplot(dat, 
       mapping = aes(a, b, color = grp)) +
  geom_point() +
  stat_smooth(dat, 
              mapping = aes(a, b, group = grp),
              method = lm, se = 1) 

# arrangieren
plot1 <- ggplot(dat,
                mapping = aes(a, b)) +
  geom_point() +
  labs(tag = 'A')

plot2 <- ggplot(dat,
                mapping = aes(a, b)) +
  geom_point()+
  labs(tag = 'B')

plot1 | plot2
plot1 | plot2 / plot1


### continuous line (ERP etc)
# surrogate data
erp = tibble(wave1 = sin(seq(-pi,pi,0.1)),
             wave2 = sin(seq(pi,-pi,-0.1)),
             timebin = seq(1,length(wave1),1))
offset    = 21 # t0 ist Punkte 21
timeBinMs = 40 # bin entspricht 40ms

# Zeitreihe
ggplot() +
  geom_line(erp, 
            mapping = aes((timebin-offset)*timeBinMs, wave2),
            color = 'blue') +
  scale_x_continuous(breaks = c(-800,-400, 400, 800, 1200, 1600),
                     name = "Time (ms)") + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-0.5,0.5),
                     name = "microVolt")

# Achsen verschieben
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
    panel.border = element_blank()) +
   coord_axes_inside(xintercept = offset, yintercept=0, labels_inside = TRUE) +
   guides(x = guide_axis_truncated(trunc_lower = -800,
                                  trunc_upper = 1600),
         y = guide_axis_truncated(trunc_lower = -0.5,
                                  trunc_upper = 0.5))

