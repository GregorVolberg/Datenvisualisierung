library(tidyverse)
library(ggplot2)

### Daten Chiara
raw <- read_delim('./2023/GruppeD/SemDiff_Chiara_Toscano.csv',
                delim = ';') %>%
      group_by(pic_name) %>%
      summarize(across(freudig:ueberraschend,
                       list(m = mean))) %>%
      ungroup() %>%
      mutate(pic = as_factor(str_sub(pic_name, 11, 14)),
             picnr = as.numeric(str_sub(pic_name, -2))) %>%
      select(pic, picnr, freudig_m:ueberraschend_m) %>%
      pivot_longer(cols = freudig_m:ueberraschend_m,
                   names_to = "emotion",
                   values_to = "m") %>%
      mutate(emotion = as_factor(str_sub(emotion, 1, -3)))
      
plt <- ggplot(raw, aes(x = emotion, y = m)) +
  geom_jitter(aes(color = pic),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_x_discrete(labels = c("freudig", "traurig", "zornig",
                              "ängstlich", "ekelhaft",
                              "überraschend"),
                   name   = "Emotionen") + 
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0, 5, by = 1),
                     name = "Mittelwert Rating")

cbPalette <- c("#009E73", "#CC79A7" ,"#999999", "#E69F00",
               "#D55E00", "#F0E442", "#0072B2","#56B4E9")

##Farbschema ändern und Anmerkungen einfügen
myPlot <- plt +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = cbPalette) +
  annotate("text", x = 5, y = 4.5,
           label = 'Kreis',
           vjust = "center", hjust = "left",
           color = cbPalette[1]) +
  annotate("text", x = 5, y = 4,
             label = 'Rechteck',
             vjust = "center", hjust = "left",
             color = cbPalette[2]) +
  annotate("text", x = 5, y = 3.5,
             label = 'Dreieck',
             vjust = "center", hjust = "left",
             color = cbPalette[3])
    
    
    

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

