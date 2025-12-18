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
    factor_A = as_factor(c(rep('rp', N/2), rep('nrp', N/2))),
    factor_B = as_factor(rep(c('0m', '4h', '24h', '7d'), N/4)),
    factor_C = as_factor(c(rep(c('group1', 'group2'), each = N/4))),
    score    = runif(N, 40, 90))))
}

data_sabrina <- function(){
  set.seed(5)
  N <- 118
  df = as_tibble(c(data.frame(
    id = as_factor(str_pad(1:(N),2, pad="0")),
    factor_A = as_factor(c(rep('Psychologie', N/2), rep('Nicht Psychologie', N/2))),
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


####### Bar plots =====================================
df   <- data_sabrina() 
ggplot(df, aes(x = factor_B,
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

#### Line plot ======================
ggplot(df, aes(x = factor_B,
               y = score,
               color  = factor_A,
               group = factor_A)) +
  geom_line(stat = "summary",
           fun.data = "mean_se",
           position = position_dodge(0.6),
           linewidth = 0.8) +
  geom_pointrange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 size = 1,
                 linewidth = 0.6)

#### ROC ======================
data = data_marina()
ggplot(data, aes(x = lure, y = target,
                 color = task)) + 
  geom_point() + 
  geom_line(stat = "summary",
            fun.data = "mean_se",
            width = 0.3)

#### Konfektionieren (Beispiel Barplot) ======================
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

plt1 +
  coord_cartesian(ylim = c(0,6)) + 
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
  theme_classic() +
  labs(x = "Merkmal", y = "Mittelwert") +
  scale_x_discrete(labels= c("X", "Y", "Z"))  + 
  #scale_x_discrete(labels= c("X", "Y", "Z"),
  #         guide = guide_axis(n.dodge = 2))  + 
  # theme(axis.text.x = element_text(angle = 30, 
  #        vjust = 0.5, hjust=0.5)) +
  theme(legend.position="inside",
        legend.position.inside = c(0.2, 0.9),
        legend.title = element_blank()) +
  annotate("text", 
           x = 2, y = 5,
           label = 'p = .034',
           vjust = 'bottom', hjust = 'center')  
  
### Reihenfolge Kategorien auf x-Achse ================
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
ggplot(df, aes(x = fct_relevel(factor_B,
                    'AV2', 'AV1', 'AV3'),
               y = score,
               color = factor_A,
               fill  = factor_A,
               group = factor_A)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4)

#### Plots arrangieren ==============================
# mit patchwork 

df <- data_sofia()
makeplots <- function(df_in){
  ggplot(data = df_in, aes(x = factor_B,
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
    theme_classic() 
}

splitted_df <- df %>% 
  group_split(factor_C)
myPlots     <- lapply(splt, FUN = makeplots)

library(patchwork)
myPlots[[1]] / myPlots[[2]]

{myPlots[[1]] + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position="inside",
          legend.position.inside = c(0.15, 0.85),
          legend.title = element_blank()) +
    labs(tag = 'Rp-', y = '% correct')
} /
{myPlots[[2]] +
      theme(legend.position = 'none') +
      labs(tag = 'Rp+', y = '% correct', x = 'Time')
}

# mit facet_wrap
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
  coord_cartesian(ylim = c(0,100))+
  facet_wrap("factor_C") +
  labs(x = 'Time', y = '% correct') +
  theme_classic() +
  theme(legend.position = c(0.1, 0.85),
        legend.title = element_blank(),
        strip.background = element_blank())
# labels für factor_C am besten im Datensatz korrekt setzen
