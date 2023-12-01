library(tidyverse)
library(ggplot2)
library(patchwork)

# surrogate data fpr Viktoriia
set.seed(12)
dat <- data.frame(WAL = 
         c(runif(10, 2, 3.4),
         runif(11, 3.41 , 4.8),
         runif(8,  4.81 , 6.2),
         runif(8,  6.21 , 7.6))) %>%
  as.tibble() %>%
  arrange(WAL) %>%
  mutate(nr = 1:n())

ggplot(dat, aes(x = nr, y = WAL)) +
  geom_path() +
  geom_point() + 
  theme_classic() + 
  scale_y_continuous(name = "Wohlbefinden-Aktivität-Laune",
                     limits = c(0,8)) + 
  scale_x_continuous(name = "Teilnehmer")
  