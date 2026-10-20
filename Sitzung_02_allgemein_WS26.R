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


# Konfektionierung Daten
# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

ggplot(mpg, aes(displ, hwy,
                color = class)) +
  geom_point() +
  scale_color_manual(values = cbPalette)

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

# Beispiel
# generiere Daten, Beispiel für zwei AVs
n   <- 95
msd <- data.frame(av = as_factor(paste0("av", 1:4)),
                  m   = c(8.24, 8.54, 0.46, 0.93),
                  std = c(0.37, 0.44, 0.06, 0.14) * sqrt(n)) # sd from se

prz <- msd %>% 
  group_by(av) %>%
  group_split() %>%
  map(~ abs(rnorm(n, .$m, .$std))) %>% # remove negative values
  simplify(.)

df <- as_tibble(data.frame(
  id      = as_factor(paste0("S",
                             str_pad(1:n, 2, pad = "0"))),
  liwc    = as_factor(rep(c("1PersonSingular",
                            "1PersonPlural"),
                          each = n*2)),
  valenz  = as_factor(rep(c("positiv", "negativ"), each = n)),
  prz     = prz)) %>%
  arrange(id)

# bar plot
set.seed(12)
df %>% filter(liwc == "1PersonSingular") %>%
  ggplot(., mapping = aes(x = valenz,
                          y = prz)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           fun.args = list(mult = 1), # 1 SE
           width = 0.3,
           alpha = 0.4,
           fill = 'white',
           color = 'black') +
  
  geom_jitter(width = 0.1,
              color = 'gray',
              size = 2) +
  geom_linerange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1),
                 size = 1) + 
  
  coord_cartesian(ylim = c(0,20),
                  xlim = c(0.7, 2.3)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Anteil Gesamtwortzahl (%)") + 
  scale_x_discrete(labels = c("positiv", "negativ"),
                   name = "Valenz")


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

# Randverteilungen
library(ggExtra)

g1 = myPlot + geom_point()
ggMarginal(g1,
           #type = 'density'
           type = 'histogram')


# Einlesen von SPSS-Dateien
library(haven) # is not loaded with tidyverse
df <- read_sav("example01.sav") %>%
  pivot_longer(cols = pre:followup,
               names_to = "test",
               values_to = "score") %>%
  mutate(across(id:test, as_factor))

df <- read_sav("example02.sav") %>%
  pivot_longer(
    cols = pre_ws:pst_ss,
    names_to = c("time", "semester"),
    names_sep = "_",
    values_to = "score") %>%
  mutate(across(id:semester, as_factor))
