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
  geom_hline(yintercept=0) +
  facet_grid(~skala) +
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
                     name = "AV") +
theme(legend.position="none",
      strip.background = element_blank())

#expand=expand_scale(add=1))
# oder 

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
  
library(ggbeeswarm)
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
  geom_quasirandom(shape = 1,
                   size=1,
                   width = 0.1,
                   dodge.width = .6, 
                   color="black",
                   alpha=.5,
                   show.legend = F) +
  scale_y_continuous(name = "Response",
                     limits = c(0, 1.25),
                     breaks = seq(0,1,0.25)) + 
  guides(y = guide_axis(cap = "upper")) +
  theme_classic() +
  theme(legend.position="none") + 
  annotate()... # hgier die sig-Sternchen


  
  
# for continuous x and y
dfsp %>% filter(Stil == "Stil1") %>%
  ggplot(., aes(x = var1, y = var2)) +
  geom_point() + 
  geom_smooth(method='lm', se = TRUE,
              level = 0.95, fullrange = TRUE,
              color = 'black') + # 95% CI
  theme_classic() +
  scale_x_continuous(expand = expansion(mult = 0.02),
                     limits=c(-3, 3),
                     name = "Emotionale Intelligenz") +
  scale_y_continuous(expand = expansion(mult = 0.02),
                     limits=c(-3, 3),
                     name = "Wert Subskala")


ggplot(dfsp, aes(x = var1, y = var2)) +
  geom_point() + 
  facet_grid(~Stil) + # ggfs switch = 'x'
  geom_smooth(method='lm', se = TRUE,
              level = 0.95, fullrange = TRUE,
              color = 'black') + # 95% CI
  theme_classic() +
  theme(legend.position="none",
        strip.background = element_blank()) + 
  scale_x_continuous(expand = expansion(mult = 0.02),
                     limits=c(-3, 3),
                     name = "Emotionale Intelligenz") +
  scale_y_continuous(expand = expansion(mult = 0.02),
                     limits=c(-3, 3),
                     name = "Wert Subskala")


# generiere Daten, 2 x 2 x 2 x 2 - Design
df = as_tibble(data.frame(
      id    = as_factor(paste0("S",
               str_pad(1:8, 2, pad = "0"))),
      training     = as_factor(rep(c("no", "yes"),
                                      each=8)),
      orientation  = as_factor(rep(c("radial", "tangential"),
                                   each = 16)),
      vfield       = as_factor(rep(c("LVF", "RVF"),
                                   each = 32)),
      time         = as_factor(rep(c("pre", "post"),
                                   each = 64)),
      score        = rnorm(8*2*2*2, 60,10))) %>%
  arrange(id, training, orientation, vfield, time)

# bar plot
set.seed(12)
df %>% filter(orientation == "radial" &
              vfield == "LVF") %>%
  ggplot(., mapping = aes(x = training,
            y = score, fill = time)) +
  geom_bar(aes(group = time), 
           stat = "summary",
           fun.data = "mean_se",
           fun.args = list(mult = 1), # 1 SE
           width = 0.5,
           position = position_dodge(0.6),
           alpha = 0.4) +
  scale_fill_manual(values = cbPalette) +
  geom_jitter(aes(color = time),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  geom_linerange(aes(group = time),
                 stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1) + 
  coord_cartesian(ylim = c(0,100),
                  xlim = c(0.7, 2.3)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(name = "Hitrate (%)") + 
  scale_x_discrete(labels = c("Kontrolle", "Training"),
                   name = "Gruppe") +
  annotate("text", x = 0.8, y = 85, 
           label = "vorher",
           vjust = "center", hjust = "center",
           color = cbPalette[1]) +
  annotate("text", x = 1.2, y = 80, 
           label = "nachher",
           vjust = "center", hjust = "center",
           color = cbPalette[2])


# patchwork
plot1 <- ggplot(mpg, aes(x=cyl, y=hwy))+
         geom_point()
plot2 <- ggplot(mpg, aes(x=cyl))+
  geom_bar()

library(patchwork)
plot1 | plot2

# mit Bezeichner (hier unnötig)
plot1 <- plot1 + labs(tag = "A")
plot2 <- plot2 + labs(tag = "B")
plot1 | plot2

# vertikal und horizontal
(plot1 | plot2) / (plot2 | plot1)


# group und map
gkeys <- df %>% 
  group_by(vfield, orientation) %>%
  group_keys()

set.seed(12)
allPlots <- df %>% 
  group_by(vfield, orientation) %>%
  group_split() %>%
  map(~
        ggplot(., mapping = aes(x = training,
                                y = score, fill = time)) +
        geom_bar(aes(group = time), 
                 stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 width = 0.5,
                 position = position_dodge(0.6),
                 alpha = 0.4) +
        scale_fill_manual(values = cbPalette) +
        geom_jitter(aes(color = time),
                    size = 2,
                    position = position_jitterdodge(
                      jitter.width = 0.2,
                      dodge.width  = 0.6)) +
        scale_color_manual(values = cbPalette) +
        geom_linerange(aes(group = time),
                       stat = "summary",
                       fun.data = "mean_se",
                       fun.args = list(mult = 1), # 1 SE
                       position = position_dodge(0.6),
                       linewidth = 1) + 
        coord_cartesian(ylim = c(0,100),
                        xlim = c(0.7, 2.3)) +
        theme_classic() +
        theme(legend.position = "none") +
        scale_y_continuous(name = "Hitrate (%)") + 
        scale_x_discrete(labels = c("Kontrolle", "Training"),
                         name = "Gruppe")
  )

allPlots[[1]] <- allPlots[[1]] +
  annotate("text", x = 0.8, y = 100, 
           label = "vorher",
           vjust = "center", hjust = "center",
           color = cbPalette[1]) +
  annotate("text", x = 1.2, y = 90, 
           label = "nachher",
           vjust = "center", hjust = "center",
           color = cbPalette[2])
(allPlots[[1]] | allPlots[[3]]) / (allPlots[[2]] | allPlots[[4]])


# bar plot, gruppiert und stacked
# see https://stackoverflow.com/questions/18774632/how-to-produce-stacked-bars-within-grouped-barchart-in-r

# generiere Daten
typ = c('Aktion', 'Refl1', 'Refl2',
        'Prot1', 'Prot2', 'Perf',
        'Rekonz')
kontext = c('neutral', 'Planung', 'Misserfolg',
            'Ausführung', 'Lage')
dfa = as_tibble(data.frame(
  Typ     = as_factor(typ),
  kontext = as_factor(rep(kontext, each = length(typ))))) %>%
  mutate(orien   = fct_recode(kontext,
                       Handlung = 'Planung',
                       Handlung = 'Misserfolg',
                       Handlung = 'Ausführung'),
         Anzahl  = round(runif(length(orien), 0, 25)))

ggplot(dfa, aes(x = Typ, y = Anzahl, fill = kontext)) + 
  geom_bar(stat = 'identity', position = 'stack') +
  facet_grid(~ orien) +
  coord_cartesian(ylim = c(0,60)) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(),
        axis.text.x  = element_text(angle=315, hjust=0))+
  scale_fill_manual(values = cbPalette) 
# legende hierzu nächste Stunde

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
