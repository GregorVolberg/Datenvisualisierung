library(tidyverse)
library(ggplot2)
library(readxl)

### Daten
set.seed(12)
dat <- data.frame(WAL = 
                    c(runif(10, 2, 3.4),
                      runif(11, 3.41 , 4.8),
                      runif(8,  4.81 , 6.2),
                      runif(2,  6.21 , 7.6))) %>%
  as_tibble() %>%
  arrange(WAL) %>%
  mutate(nr = 1:n())

#### plotting 
ggplot(dat, aes(x = WAL, y = nr)) +
  geom_point() + 
  geom_line() +
  theme_classic() + 
  scale_x_continuous(
    name = "Wohlbefinden-Aktivität-Laune",
    limits = c(0,8)) + 
  scale_y_continuous(
    name = "Teilnehmer") +
  geom_vline(
    xintercept = c(2, 3.4, 4.8, 6.2, 7.6),
    linetype = 'dashed')+
  annotate("rect", xmin = 2, xmax = 3.4,
           ymin = 0, ymax = 10,
           alpha = .1,fill = "red")+
  annotate("rect", xmin = 3.41, xmax = 4.8,
           ymin = 0, ymax = 11,
           alpha = .1,fill = "red")+
  annotate("rect", xmin = 4.81, xmax = 6.2,
           ymin = 0, ymax = 8,
           alpha = .1,fill = "red")+
  annotate("rect", xmin = 6.21, xmax = 7.6,
           ymin = 0, ymax = 2,
           alpha = .1,fill = "red")




