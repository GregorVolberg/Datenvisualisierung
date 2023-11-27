library(tidyverse)
library(patchwork)
library(ggplot2)
library(magrittr)


### Achsen
dat <- read_csv(file = 'Julia.csv') %>%
  mutate(prepost = as_factor(prepost),
         n_sessions = as_factor(n_sessions))

# cbPalette
# use color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

plot1 <- ggplot(dat, aes(n_sessions, OPP)) +
  geom_jitter(aes(color = prepost),
                  position = position_jitterdodge(
                    jitter.width = 0.2,
                    dodge.width = 0.6)) +
  stat_summary(aes(group = prepost),
               data = dat,
               fun.data = "mean_se",
               fun.args = list(mult = 1.5),
               linewidth = 1,
               position=position_dodge(0.6)) +
  theme_classic() + 
  theme(strip.background = element_blank(),
        legend.position = "none") +
  scale_color_manual(values = cbPalette) +
  scale_x_discrete(name = "Anzahl Sitzungen") + 
  scale_y_continuous(limits = c(0, 1.2),
                     breaks = seq(0,1,0.2),
                     name = "Mittlere Reaktionszeit (s)") +
  labs(tag = "A") +
  annotate("text", x = 0.7, 1.1,
           label = "pre",
           color = cbPalette[1]) +
  annotate("text", x = 1.2, 1.1,
           label = "post",
           color = cbPalette[2]) 
  
plot2 <- ggplot(dat, aes(n_sessions, PRL)) +
  geom_jitter(aes(color = prepost),
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width = 0.6)) +
  stat_summary(aes(group = prepost),
               data = dat,
               fun.data = "mean_se",
               fun.args = list(mult = 1.5),
               position=position_dodge(0.6)) +
  theme_classic() + 
  scale_color_manual(values = cbPalette) +
  theme(strip.background = element_blank(),
        legend.position = "none") +
  scale_x_discrete(name = "Anzahl Sitzungen") + 
  scale_y_continuous(limits = c(0, 1.2),
                     breaks = seq(0,1,0.2),
                     name = "Mittlere Reaktionszeit (s)") +
  labs(tag = "B") +
  annotate("text", x = 0.8, 1.1,
           label = "pre",
           color = cbPalette[1]) +
  annotate("text", x = 1.2, 1.1,
           label = "post",
           color = cbPalette[2])
  

plot1|plot2

