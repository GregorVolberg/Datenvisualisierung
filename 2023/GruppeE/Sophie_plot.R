##laden
library(tidyverse)
library(readxl)
#library(foreign)

##Einlesen
filename <- "UNI/bachelor/Abbildungen/MLR.xlsx"
raw <- read_xlsx(filename,
                 range = "E3:F62",
                 col_names = c("objektiv","vsro")) %>%
  bind_cols(
    read_xlsx(filename,
              range = "E69:F128",
              col_names = c("subKog",
                            "vsrsk"))) %>%
  bind_cols(
    read_xlsx(filename,
              range = "E133:F192",
              col_names = c("subPhys",
                            "vsrsp"))) %>%
  as_tibble()


# Abbildung 4b: objektive Anstrengungskosten

myPlot1 <- ggplot(raw, aes(x = objektiv, y = vsro)) +
  geom_point(shape = 1,
             color = 'black',
             size = 2) +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  annotate("text", x = 600, y = 7,
           label = 'ß = -0.43',
           vjust = "center", hjust = "left") +
  coord_cartesian(xlim = c(-500,900),
                  ylim = c(-10, 25)) +
  labs(x = "objektive Anstrengungskosten (ms)",
       y = "Voluntary Switch Rate (%)",
       tag = "A") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-500, 900, by = 200)) +
  scale_y_continuous(breaks = seq(-10, 25, by = 5))




##Abbildung 5b: subjektiv kognitive Anstrengungskosten

myPlot2 <- ggplot(raw, aes(x = subKog, y = vsrsk)) +
  geom_point(shape = 1,
             color = 'black',
             size = 2) +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  annotate("text", x = 1.25, y = 7,
           label = 'ß = -0.12',
           vjust = "center", hjust = "left") +
  coord_cartesian(xlim = c(-1,2),
                  ylim = c(-10, 25)) +
  labs(x = "subjektiv kognitive Anstrengungskosten (€)",
       y = "Voluntary Switch Rate (%)",
       tag = "B") +
  theme_classic() + 
  scale_x_continuous(breaks = seq(-1, 2, by = 0.5)) +
  scale_y_continuous(breaks = seq(-10, 25, by = 5))


## Abbildung 6b: subjektiv phyische Anstrengungskosten
myPlot3 <- ggplot(raw, aes(x = subPhys, y = vsrsp)) +
  geom_point(shape = 1,
             color = 'black',
             size = 2) +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  annotate("text", x = 1.25, y = 7,
           label = 'ß = -0.02',
           vjust = "center", hjust = "left") +
  coord_cartesian(xlim = c(-1,2),
                  ylim = c(-10, 25)) +
  labs(x = "subjektiv physische Anstrengungskosten (€)",
       y = "Voluntary Switch Rate (%)",
       tag = "C") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-1, 2, by = 0.5)) +
  scale_y_continuous(breaks = seq(-10, 25, by = 5))


# bennen
plot1 <- myPlot1 + labs(tag = "A")
plot2 <- myPlot2 + labs(tag = "B")
plot3 <- myPlot3 + labs(tag = "C")


#arrangieren
plot1 | plot2 | plot3  
##irgendwie Fehlermelung??

#arrangieren
library(gridExtra)

myPlot = grid.arrange(plot1, plot2, plot3, ncol = 3)
