library(tidyverse)

ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point()

# Kurzform
ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# mapping Drittvariable
ggplot(mpg, aes(displ, hwy,
                color = class)) +
  geom_point()


# Konfektionierung Daten
ggplot(mpg, aes(displ, hwy)) +
  geom_point(shape = 8,
             color = 'red',
             size = 4)


# Konfektionierung Daten: shape
ggplot(mpg, aes(displ, hwy,
                shape = class)) +
  geom_point() +
  scale_shape_manual(values = 1:7)


### Achsen
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  coord_cartesian(xlim = c(0,10), ylim = c(0, 45))+
  scale_x_continuous(breaks = c(2,6),
                     minor_breaks = seq(0,10,0.5),
                     labels = c("wenig", "viel"),
                     name = "Displacement") + 
  scale_y_continuous(breaks = seq(0,50,10),
                     minor_breaks = seq(0,45,5),
                     name = "Mileage") 

# Anmerkungen
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_hline(yintercept = 35,
             linetype = 'dashed') +
  annotate("rect", xmin = 1, xmax = 2,
           ymin = 20, ymax = 50,
           alpha = .1,fill = "blue") +
  annotate("text", x = 2.5, y = 40,
           label = 'Economic cars',
           vjust = "center", hjust = "left")

# themes
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  coord_cartesian(xlim = c(0,10), ylim = c(0, 45))+
  theme_classic() +
  theme(
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.x = element_line(color = 'red'))

# Objekt
myPlot <- ggplot(mpg, aes(displ, hwy)) 
myPlot + geom_point()

# plots arrangieren
library(patchwork)

g1 = myPlot + geom_point()
g2 = myPlot + geom_point() + geom_smooth()

g1 | g2
g1 / g2

g1 <- g1 + labs(tag = "A")
g2 <- g2 + labs(tag = "B")
g1 | g2

g1 + g2 + plot_layout(widths = c(1,2))


# Einlesen von SPSS-Dateien
library(haven) # is not loaded with tidyverse
df <- read_sav("example01.sav") %>%
  pivot_longer(cols = pre:followup,
               names_to = "time",
               values_to = "score") %>%
  mutate(across(id:time, as_factor))

df <- read_sav("example02.sav") %>%
  pivot_longer(
    cols = pre_ws:pst_ss,
    names_to = c("time", "semester"),
    names_sep = "_",
    values_to = "score") %>%
  mutate(across(id:semester, as_factor))
