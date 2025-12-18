library(tidyverse)

data_katja <- function(){
  set.seed(5)
  N <- 100
  df = as_tibble(c(data.frame(
    id = as_factor(str_pad(1:(N),2, pad="0")),
    factor_A = as_factor(c(rep('Frau', N/2), rep('Mann', N/2))),
    AV1 = sample(1:7, N, replace = TRUE),
    AV2 = sample(1:7, N, replace = TRUE),
    AV3 = sample(1:7, N, replace = TRUE)))) %>%
    pivot_longer(., AV1:AV3, names_to = 'factor_B', values_to = 'score') %>%
    mutate(factor_B = as_factor(factor_B))
}

data_sofia <- function(){
  set.seed(5)
  N <- 120
  df = as_tibble(c(data.frame(
    id = as_factor(str_pad(1:(N),2, pad="0")),
    factor_A = as_factor(c(rep('rp+', N/4), rep('rp-', N/4), rep('nrp', N/2))),
    factor_B = as_factor(rep(c('0m', '4h', '24h', '7d'), N/4)),
    factor_C = as_factor(c(rep(c('group1', 'group2'), each = N/4))),
    score    = runif(N, 40, 90))))
}

data_sabrina <- function(){
  set.seed(5)
  N <- 118
  df = as_tibble(c(data.frame(
    id = as_factor(str_pad(1:(N),2, pad="0")),
    factor_A = as_factor(c(rep('Psych', N/2), rep('NoPsych', N/2))),
    AV1 = runif(N, 1.9, 4.8),
    AV2 = runif(N, 1.9, 4.8),
    AV3 = runif(N, 1.9, 4.8)))) %>%
    pivot_longer(., AV1:AV3, names_to = 'factor_B', values_to = 'score') %>%
    mutate(factor_B = as_factor(factor_B))
}

data_anushe <- function(){
  set.seed(5)
  N <- 20*3*4
    df = as_tibble(c(data.frame(
      id = as_factor(str_pad(1:(N),2, pad="0")),
      factor_A = as_factor(rep(c('group1', 'group2', 'group3'),each = N/3)),
      factor_B = as_factor(rep(c('pre', 't1', 't2', 't3'), N/4)),
      AV1 = runif(N/3/4, 1, 3) + rep(c(0,1,2), each=N/3),
      AV2 = runif(N/3/4, 1, 3) + rep(c(0,1,2), each=N/3),
      AV3 = runif(N/3/4, 1, 3) + rep(c(0,1,2), each=N/3)))) %>%
      pivot_longer(., AV1:AV3, names_to = 'factor_C', values_to = 'score') %>%
      mutate(factor_C = as_factor(factor_C))
}

data_marina <- function(){
  read_csv2('Froeber2025.csv') %>%
    pivot_longer(., SureTarget:SureLure, 
                 names_to = 'criterionValue', 
                 values_to = 'score') %>%
    pivot_wider(., names_from=type, values_from = score) %>%
    filter(criterionValue != "SureLure")
}

# color-blind friendly palette, see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")


#######====================== Bar plots
df <- data_sabrina() 

plt1 <- ggplot(df, aes(x = factor_B,
               y = score,
               fill  = factor_A,
               group = factor_A)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4) +
  geom_jitter(aes(color = factor_A),
              size = 1.5,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  geom_linerange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1,
                 color = 'black')

# konfektionieren
plt1 +
  coord_cartesian(ylim = c(0,6)) + 
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
  theme_classic() +
  labs(x = "Merkmal", y = "Mittelwert") +
  scale_x_discrete(labels= c("X", "Y", "Z"))  + 
  #scale_x_discrete(labels= c("very long label A", "very long label B", "very long label C"),
  #                 guide = guide_axis(n.dodge = 2))  + 
  # theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="inside",
        legend.position.inside = c(0.15, 0.9),
        legend.title = element_blank()) +
  annotate("text", 
           x = 2, y = 5,
           label = 'p = .034',
           vjust = 'bottom', hjust = 'center')  
  
######======== Reihenfolge Kategorien auf x-Achse
# Original:
ggplot(df, aes(x = factor_B,
               y = score,
               fill  = factor_A,
               group = factor_A)) +
  geom_bar(stat = "summary",
          fun.data = "mean_se",
          width = 0.3,
          position = position_dodge(0.6),
          alpha = 0.4)

# Modifiziert:
ggplot(df, aes(x = fct_relevel(factor_B, 'AV2', 'AV1', 'AV3'),
               y = score,
               color = factor_A,
               fill  = factor_A,
               group = factor_A)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4)

####========== Plots arrangieren


## Katja
df <- data_katja()

ggplot(df, aes(x = factor_B,
               y = score,
               color = factor_A,
               fill  = factor_A,
               group = factor_A)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4) +
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
#  geom_jitter(size = 1,
#              position = position_jitterdodge(
#                jitter.width = 0.2,
#                dodge.width  = 0.6))
  geom_linerange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1,
                 color = 'black') + 

  coord_cartesian(ylim = c(0,7)) +
  theme_classic() +
  theme(legend.position="inside",
        legend.position.inside = c(0.15, 0.85),
        legend.title = element_blank()) +
  #theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
  labs(x = "Merkmal", y = "Score") + 
  scale_x_discrete(labels= c("very very long label A", "long label B", "long long label C"),
                   guide = guide_axis(n.dodge = 2))  + 
  annotate("text", x = 1,
           y = 4.2,
           label = c('p = .034'),
           vjust = 'bottom', hjust = 'center')  

# legende position ändern und x-Achse
df %>%
  summarize(m = mean(score),
                 .by = c(factor_A, factor_B)) %>%
  pivot_wider(names_from = factor_A, values_from = m) %>%
  mutate(Differenz = Mann - Frau) %>%
  arrange(Differenz) %>%
  print()

df %>% mutate(factor_B = fct_relevel(.$factor_B, 'AV2', 'AV1', 'AV3')) %>%
    ggplot(aes(x = factor_B,
               y = score,
               color = factor_A,
               fill  = factor_A,
               group = factor_A)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4) + 
  theme_classic()


## Sofia
df <- data_sofia()

library(patchwork)
plot1 <- df %>%
  filter(factor_C == 'group1') %>%
  ggplot(aes(x = factor_B,
           y = score,
           color = factor_A,
           fill  = factor_A,
           group = factor_A)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4) +
  labs(tag = 'Rp-') + 
  coord_cartesian(ylim = c(0,100))+
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position="inside",
        legend.position.inside = c(0.15, 0.85),
        legend.title = element_blank())
  
plot2 <- df %>%
  filter(factor_C == 'group2') %>%
  ggplot(aes(x = factor_B,
             y = score,
             color = factor_A,
             fill  = factor_A,
             group = factor_A)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4) +
  coord_cartesian(ylim = c(0,100))+
  labs(tag = 'Rp+') + 
  theme_classic() +
  theme(legend.position = 'none')

plot1/plot2



## Anushe
df = data_anushe()
df %>% ggplot(aes(x =factor_B, 
                  y = score,
                  color = factor_A,
                  group = factor_A)) +
  geom_line(stat = "summary",
            fun.data = "mean_se",
            linewidth = 1,
            position = position_dodge(0.6)) +
  geom_pointrange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1,
                 size = 0.8) +
  facet_wrap("factor_C") +
  theme_classic() +
  coord_cartesian(ylim = c(0,5)) +
  theme(legend.position = c(0.15, 0.13),
        legend.title = element_blank(),
        strip.background = element_blank())
        #strip.text.x = element_blank())



## Marina
data = data_marina()
ggplot(data, aes(x = lure, y = target, color = task)) + 
  geom_point() + 
  geom_line(stat = "summary",
            fun.data = "mean_se",
            width = 0.3)




