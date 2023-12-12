library(tidyverse)
library(ggplot2)
library(patchwork)
library(readxl)

# surrogate data for Viktoriia
set.seed(12)
dat <- data.frame(WAL = 
         c(runif(10, 2, 3.4),
         runif(11, 3.41 , 4.8),
         runif(8,  4.81 , 6.2),
         runif(2,  6.21 , 7.6))) %>%
  as_tibble() %>%
  arrange(WAL) %>%
  mutate(nr = 1:n())

ggplot(dat, aes(x = nr, y = WAL)) +
  geom_point() + 
  geom_line() +
  theme_classic() + 
  scale_y_continuous(
    name = "Wohlbefinden-Aktivität-Laune",
    limits = c(0,8)) + 
  scale_x_continuous(
    name = "Teilnehmer") +
  geom_hline(
    yintercept = c(2, 3.4, 4.8, 6.2, 7.6),
    linetype = 'dashed')
  
# alternativ
ggplot(dat, aes(x = nr, y = WAL)) +
  geom_point() + 
  geom_line() +
  theme_classic() + 
  scale_y_continuous(
    name = "Wohlbefinden-Aktivität-Laune",
    limits = c(0,8)) + 
  scale_x_continuous(
    name = "Teilnehmer") +
  annotate('rect',
           xmin = 0.2, xmax = 10.2,
           ymin = 2, ymax = 3.4,
           alpha = .3) # etc


# Helena
raw <- read_excel('./2023/GruppeC/Gruppenwerte_Helena.xlsx',
                  sheet = "Mono vs. Bi",
                  range = "A3:E33") %>%
       mutate(condition = "monokular") %>%  
       bind_rows(read_excel('./2023/GruppeC/Gruppenwerte_Helena.xlsx',
                            sheet = "Mono vs. Bi",
                            range = "F3:J33")) %>%
       rename(tmp = `...1`) %>%
       mutate(condition = as.factor(if_else(is.na(condition),
                                  "binokular",
                                  "monokular")),
              VP  = replace_na(VP, 0),
              tmp = replace_na(tmp, 0),
              VP = as.factor(VP + tmp)) %>%
       pivot_longer(cols = `Training 1`:`Training 4`,
               names_to = "session",
               values_to = "threshold") %>%
       mutate(session = as.factor(str_sub(session, 10, 10))) %>%
       select(-tmp) %>%
       summarise(mthreshold = mean(threshold), .by = c(VP, condition, session))  

ggplot(raw, aes(x = session, y = mthreshold)) +
  geom_jitter(aes(color = condition),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  stat_summary(
    aes(group = condition),
    fun.data="mean_se",
    fun.args = list(mult = 1.5), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.6)) +
    theme_classic()  
  
# schöner
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

ggplot(raw, aes(x = session, y = mthreshold)) +
  geom_jitter(aes(color = condition),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  stat_summary(
    aes(group = condition),
    fun.data="mean_se",
    fun.args = list(mult = 1.5), 
    size = 1,
    linewidth = 1,
    position = position_dodge(0.6),
    geom = 'line', 
    colour = c(rep(cbPalette[1],4),rep(cbPalette[2],4))) +
  stat_summary(
    aes(group = condition),
    fun.data="mean_se",
    fun.args = list(mult = 1.5), 
    size = 0.6,
    linewidth = 1,
    position = position_dodge(0.6)) + 
  theme_classic() + 
  scale_x_discrete(
    name = "Sitzung") +
  scale_y_continuous(
    name = "Schwelle",
    limits = c(0,23)) +
  theme(legend.position = "none") +
  annotate("text", x = 0.9, y = 22,
           label = 'monokular',
           vjust = "center", hjust = "center",
           color = cbPalette[1]) + 
  annotate("text", x = 1.1, y = 21,
           label = 'binokular',
           vjust = "center", hjust = "center",
           color = cbPalette[2]) 

# p-values
  ... +
  annotate("segment",
           x = 0.8, xend = 1.2,
           y = 13, yend = 13) +
  annotate("text",
           x = 1,
           y = 13.3,
           label = "p = .034")


# arrangieren
plot1 <- ggplot(....) +
         labs(tag = "A")
plot2 <- ggplot(...) +
         labs(tag = "B")

plot1 | plot2
plot1 | plot2 / plot1

# Natalie
rawN <- read_excel('./2023/GruppeC/Mittelwerte_Ergebnisse_Bachelorarbeit_Nathalie_Zeus.xlsx',
           range = "A1:D10") %>%
  rename(tmp = `...1`,
         std = `Std.-Abweichung`) %>%
  na.omit() 

indData = matrix(NA, nrow = dim(rawN)[1], ncol = rawN$N[1])
for(j in 1:dim(rawN)[1]){
  indData[j,] = rnorm(rawN$N[j],rawN$Mittelwert[j],rawN$std[j])
}

rawN2 <- bind_cols(rawN, as_tibble(indData)) %>%
  select(-Mittelwert,-std, -N) %>%
  pivot_longer(cols = V1:V31, names_to = "vp", values_to = "switchRate") %>%
  separate_wider_delim(cols = tmp, delim =  '.', names=c('context', 'sequence')) %>%
  mutate(across(context:vp, as_factor)) %>%
  select(vp, context, sequence, switchRate)
