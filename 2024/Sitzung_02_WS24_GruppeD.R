# Sitzung 02, WiSe 2024, Gruppe D
library(tidyverse)

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# # To use for fills, add
# scale_fill_manual(values=cbPalette)
# # To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

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

# plots arrangieren
library(patchwork)
p1 <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram()
p2 <- ggplot(mpg, aes(x = hwy,
                      y = cty)) +
  geom_point()

# vertikal
p1 / p2

# horizontal
p1 | p2

# vertikal und horizontal
(p1 | p2) / (p1)

# mit Bezeichner
p1 <- p1 + labs(tag = "A")
p2 <- p2 + labs(tag = "B")
p1 / p2

# generiere Daten
df <- as_tibble(data.frame(
  id      = as_factor(paste0("S",
                     str_pad(1:60, 2, pad = "0"))),
  skala = as_factor(c(rep("vitalitaet", 60),
                      rep("koerperkontakt", 60))),
  score = as_factor(c(rep("negativ", 10),
                      rep("positiv", 50),
                      rep("negativ", 20),
                      rep("positiv", 40)))))
  
ggplot(df, mapping = aes(x = skala, fill = score)) +
  geom_bar(aes(group = score),
           position = position_dodge(0.5),
           width = 0.4) +
  scale_fill_manual(values = cbPalette) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Anzahl") + 
  scale_x_discrete(labels = c("Vitalität",
                              "Körperlichkeit"),
                   name = "") +
  annotate("text", x = 1.5, y = 50, 
           label = "positiv",
           vjust = "center", hjust = "left",
           color = cbPalette[1]) +
  annotate("text", x = 1.5, y = 45, 
           label = "negativ",
           vjust = "center", hjust = "left",
           color = cbPalette[2])


## Scatter plot
# generiere Daten
n <- 30
df <- as_tibble(data.frame(
  id           = as_factor(paste0("S",
                  str_pad(1:30, 2, pad = "0"))),
  bedingung    = as.factor(sample(1:3, n, replace = TRUE)),
  note         = sample(1:6, n, replace = TRUE),
  erwartung    = sample(1:6, n, replace = TRUE)))

ggplot(df, aes(x = note, y = erwartung, 
               group = bedingung)) +
  geom_smooth(aes(color = bedingung,
                  fill = bedingung),
              method = 'lm',
              se = TRUE,
              level = 0.95,
              formula = y~poly(x,2),
              fullrange = TRUE) +
  geom_point(aes(color = bedingung), size = 2) + 
  scale_color_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  coord_cartesian(ylim = c(-4,10),
                  xlim = c(1, 6)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Erwartung",
                     breaks = c(1:6)) + 
  scale_x_continuous(name = "Note",
                     breaks = c(1:6))
                    
## Matrix plot
# generiere Daten
n <- 30
df <- as_tibble(data.frame(
  id         = as_factor(paste0("S",
               str_pad(1:30, 2, pad = "0"))),
  risiken    = sample(1:7, n, replace = TRUE),
  vertrauen1 = sample(1:7, n, replace = TRUE),
  vertrauen2 = sample(1:7, n, replace = TRUE),
  alter      = sample(18:80, n, replace = TRUE),
  operator   = sample(1:7, n, replace = TRUE)))

library(GGally)
ggpairs(df, 
        columns = 2:6,
        lower = list(continuous = wrap('smooth',
                          color = 'blue',
                          size = 1.5)),
        diag  = list(continuous = wrap(ggally_barDiag,
                                       binwidth = 1)),
        upper = list(continuous = wrap(ggally_cor,
                                       title = "r",
                                       size = 3))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background= element_blank())

# ggfs: ggplot-Objekt erstellen und Darstellung für Alter korrigieren
plt <- ggpairs(df, 
        columns = 2:6,
        lower = list(continuous = wrap('smooth',
                                       color = 'blue',
                                       size = 1.5)),
        diag  = list(continuous = wrap(ggally_barDiag,
                                       binwidth = 1)),
        upper = list(continuous = wrap(ggally_cor,
                                       title = "r",
                                       size = 3)))

plt[4,4] <- ggplot(df, aes(x = alter)) + geom_density()


# Einlesen von SPSS-Dateien
library(haven) # is not loaded with tidyverse
df <- read_sav("example01.sav") %>%
              pivot_longer(cols = pre:followup,
                           names_to = "test",
                           values_to = "score") %>%
              mutate(across(id:test, as_factor))

# Zweifaktorielle Messwiederholung
df <- read_sav("example02.sav") %>%
  pivot_longer(
    cols = pre_ws:pst_ss,
    names_to = c("time", "semester"),
    names_sep = "_",
    values_to = "score") %>%
  mutate(across(id:semester, as_factor))