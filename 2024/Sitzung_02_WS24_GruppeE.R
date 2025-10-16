# Sitzung 02, WiSe 2024, Gruppe E
library(tidyverse)

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# To use for fills: scale_fill_manual(values=cbPalette)
# To use for line and point colors: scale_colour_manual(values=cbPalette)


# generiere Daten für Häufigkeitsdiagramm
dfh <- as_tibble(
  data.frame(f       = round(c(runif(6,5,44),
                               runif(6,2,29))),
             Gruppe  = as_factor(rep(c("Epilepsie", "Kontrolle"),
                           each = 6)),
             Emotion = rep(c("Trauer", "Wut",
                          "Angst", "Ekel",
                          "Überraschung", "Freude"), 2)))   

dfh %>%
  ggplot(., mapping = aes(x = Emotion, y = f/60, 
                          fill = Gruppe)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.6),
           width = 0.5,
           alpha = 0.4) +
  scale_fill_manual(values = cbPalette) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  scale_y_continuous(name = "Anteil korrekter Antworten") + 
  theme(legend.position = c(0.1, 0.8),
        legend.justification = c('left', 'center'),
        legend.title = element_blank()) 
# theme(legend.position = "none") +
  #   annotate("text", x = 1.2, y = 0.7, 
  #          label = "Epilepsie",
  #          vjust = "left", hjust = "center",
  #          color = cbPalette[1]) +
  # annotate("text", x = 1.5, y = 0.63, 
  #          label = "Kontrolle",
  #          vjust = "left", hjust = "center",
  #          color = cbPalette[2])


# generiere Daten für Streudiagramm
dfs <- as_tibble(
  data.frame(skala   = rep(c("Sozial", "Geschwister",
                         "Finanziell", "Persönlich",
                         "Bewältigung"), each= 55),
             score   = rep(rnorm(55),5),
             av      = rnorm(55)))

dfs %>% ggplot(., mapping = aes(x = score, y = av)) +
  #facet_grid(~skala) +
  geom_smooth(method='lm',
              se = TRUE,
              level = 0.95,
              fullrange = TRUE,
              color = 'black',
              fill = 'gray') + # 95% CI
  geom_point() +
  theme_classic() +
  scale_x_continuous(limits=c(-3, 3),
                     name = "Skala") +
  scale_y_continuous(limits=c(-3, 3),
                     name = "AV") #+
#theme(legend.position="none",
#      strip.background = element_blank())

##### ===== Violin Plot

dfv <- as_tibble(
  data.frame(request  = as_factor(rep(c("deliberate",
                                        "assertive"),
                                      each= 20)),
             group    = as_factor(rep(c("Regensburg",
                                        "Ibadan"),
                                      each= 10)),
             response = sample(seq(0,1,0.25), 40,
                               replace = TRUE)
             )
  )
  

dfv %>% ggplot(., mapping = aes(x = group, y = response,
                                fill = request)) +
  geom_violin(position = position_dodge(0.6),
               alpha = 0.4,
               width = 0.5,
               color = NA,
               bw = 0.15) +
  stat_summary(aes(fill = request),
               position = position_dodge(0.6),
               fun = "mean",
               geom = "point",
               pch = 21,
               alpha = 0.8,
               size = 3) +
  scale_fill_manual(values = cbPalette) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5,
               position = position_dodge(0.6),
               color = NaN) +
  scale_y_continuous(name = "Response",
                     limits = c(0, 1.25),
                     breaks = seq(0,1,0.25)) + 
  guides(y = guide_axis(cap = "upper")) +
  theme_classic() 
#
#  theme(legend.position="none")
# ggfs.  annotate()... für die sig-Sternchen, Legende, ...



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
