library(tidyverse)
library(ggplot2)
#library(readxl)
library(ggforce)


### Daten Chiara
raw <- read_delim('./2023/GruppeD/SemDiff_Chiara_Toscano.csv',
                delim = ';') %>%
      group_by(pic_name) %>%
      summarize(across(freudig:ekelhaft, list(m = mean)))

library(psych)
pc <- raw %>% 
  select(where(is.numeric)) %>%
  principal(., nfactors=3, rotate='varimax', scores=T) 
# export
bind_cols(raw$pic_name, pc$scores) %>%
  mutate(type = str_sub(...1, 11, 17),
         num  = str_sub(...1, -2, -1)) %>%
  select(type, num, F1 = RC1, F2 = RC2, F3 = RC3) %>%
  write_csv('chiara_fscores.csv')

  

  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE)

         mutate(vp = as_factor(vp),
              sex = fct_recode(as_factor(sex),
                               m = "mÃ¤nnlich", f = "weiblich"),
              pic_type = as_factor(pic_type),
              pic_name = as_factor(pic_name))
# pca
pc <- raw %>% 
      group_by(pic_name) %>%
      summarize(across(freudig:ekelhaft, list(m = mean)))

raw %>% group_by(pic_name) %>%
  summarize()
      

# mitteln und dann PCA
# pca mit srtip plot

##### Darstellung
Jorah <- data.frame(
  x0  = c(10, 10, 30),
  y0  = c(10, 30, 20),
#  x1  = c(10, 30, 30),
#  y1  = c(30, 20, 20),
  r   = rep(2, 3), # radius
  lab = c("PF", "PR", "IP") # label
)

ggplot() +
  geom_link(aes(x = c(10, 10, 10),
                y = c(10, 10, 30),
                xend = c(10, 30, 30),
                yend = c(30, 20, 20))) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r), # gehört zu ggforce
              data = Jorah,
              size = 1,
              fill = "white",
              show.legend = FALSE) +
  scale_x_continuous(limits = c(0,40)) +
  scale_y_continuous(limits = c(0,40)) +
  annotate("text",
           x = Jorah$x0,
           y = Jorah$y0,
           label = Jorah$lab) +
  geom_label(aes(x = c(20, 10, 20),
                 y = c(15, 20, 25),
                 label = c("0.9", "0.29", "0.03")),
             label.size = 0,
                 fill = "white") +
  theme_void()

