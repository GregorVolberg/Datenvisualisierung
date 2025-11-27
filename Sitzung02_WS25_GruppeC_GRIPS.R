# WS25, Gruppe C
library(tidyverse)

# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# To use for fills, add
# scale_fill_manual(values=cbPalette)
# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

# Paul
# generate data
n <- 40
KI         <- rnorm(n, 5,2)
sozEingeb  <- KI + rnorm(n, 5,3)
traitAngst <- sozEingeb + rnorm(n, -3,1)
df = as_tibble(data.frame(
  id      = as_factor(paste0("S", str_pad(1:n, 2, pad = "0"))),
  KI,
  sozEingeb,
  traitAngst))

# add binned variable KI_category
df <- df %>% mutate(KI_category = 
         as_factor(case_when(
           KI < quantile(KI, 1/3) ~ "low",
           KI > quantile(KI, 2/3) ~ "high",
           .default = "medium"
         )))

ggplot(df, aes(x = traitAngst, 
               y = sozEingeb,
               color = KI_category)) +
  geom_smooth(method='lm', se = TRUE,
              level = 0.95,  
              fullrange = TRUE) + # 95% CI
  geom_point() + 
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
  theme_classic() +
  labs(x = 'Trait-Angst', y = 'Soziale Eingebundenheit', color = 'KI') + 
  theme(legend.position="inside",
        legend.position.inside = c(0.1, 0.8)) + 
  coord_cartesian(ylim = c(0,20),
                  xlim = c(0,12)) 


## ASTRID
n  <- 46
id        <- as_factor(1:n)
trial     <- as_factor(1:5)
condition <- as_factor(1:4)
df = as_tibble(data.frame(
  id      = rep(id, each = length(trial) * length(condition)),
  trial   = rep(trial, length(condition)),
  condition = rep(condition, each = length(trial)),
  bis_error = rnorm(n * length(trial) * length(condition))))


library(ggExtra)

plt1 <- df %>% 
  filter(condition == 1) %>%
  group_by(id) %>%
  mutate(m_error = mean(bis_error)) %>%
  ungroup() %>%
  ggplot(., aes(x = bis_error, y = reorder(id, m_error))) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_point(color = 'gray80') +
  #geom_point(aes(color = trial)) +
  #scale_color_manual(values = c('gray30', 'gray40', 'gray50', 'gray60', 'gray70')) +
  geom_pointrange(stat = "summary",
                  fun.data = "mean_se",
                  fun.args = list(mult = 1), # 1 SE
                  #position = position_dodge(0.6),
                  linewidth = 1,
                  color = 'black') +
  theme_classic() +
  coord_cartesian(xlim = c(-2.5,2.5)) + 
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y  = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = 'Bisection Error (%)', 
       title = "Baseline")  +
  annotate("text", x = 2, y = 5,
           label = c('p = .021'),
           vjust = 'bottom', hjust = 'center')
ggMarginal(plt1, groupColour = TRUE, groupFill = TRUE, margins = "x")


condition_labels <- c('normal', 'left swipe', 'right swipe', 'blind')
p_labels <- c('.236', '.564', '.823', '.197')
pplt <- list(4) # allocate
for (k in 1:4){
  plt1 <- df %>% 
    filter(condition == k) %>%
    group_by(id) %>%
    mutate(m_error = mean(bis_error)) %>%
    ungroup() %>%
    ggplot(., aes(x = bis_error, y = reorder(id, m_error))) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_point(color = 'gray80') +
    #geom_point(aes(color = trial)) +
    #scale_color_manual(values = c('gray30', 'gray40', 'gray50', 'gray60', 'gray70')) +
    geom_pointrange(stat = "summary",
                    fun.data = "mean_se",
                    fun.args = list(mult = 1), # 1 SE
                    #position = position_dodge(0.6),
                    linewidth = 1,
                    color = cbPalette[k]) +
    theme_classic() +
    coord_cartesian(xlim = c(-2.5,2.5)) + 
    theme(axis.text.y  = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y  = element_blank(),
          axis.title.y = element_blank()) +
    labs(x = 'Bisection Error (%)', 
         title = condition_labels[k])  +
    annotate("text", x = 2, y = 5,
             label = c('p = .021'),
             vjust = 'bottom', hjust = 'center')
pplt[[k]] <- ggMarginal(plt1,
                        groupColour = TRUE,
                        groupFill = TRUE,
                        margins = "x")
}

library(patchwork)
(pplt[[1]]|pplt[[2]])/(pplt[[3]]|pplt[[4]])
  
ggMarginal(plt1, groupColour = TRUE, groupFill = TRUE, margins = "x")

# see https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r


## PAULA
N1 <- 100
N2 <- 41
sigma1  <- rbind(c(1,0.342), c(.342,1)) # correlation matrix
sigma2  <- rbind(c(1,0.192), c(.192,1)) # correlation matrix
mu      <- c(10, 10) # mean of variables
corrmat <- rbind(MASS::mvrnorm(n = N1, mu=mu,
                               Sigma=sigma1,
                               empirical = TRUE),
                 MASS::mvrnorm(n = N2, mu=mu,
                               Sigma=sigma2,
                               empirical = TRUE))

df = as_tibble(c(data.frame(
  id = as_factor(str_pad(1:(N1+N2),3, pad="0")),
  Geschlecht = as_factor(c(rep('Frau', N1), rep('Mann', N2))),
  corrmat))) %>%
  rename(Fuehrungsstil = 'X1', Wohlbefinden = 'X2')

# wie Paul
ggplot(df, aes(x = Fuehrungsstil,
               y = Wohlbefinden,
               color = Geschlecht)) +
  geom_smooth(method='lm', se = TRUE,
              level = 0.95,  
              fullrange = TRUE) + # 95% CI
  geom_point() + 
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
  theme_classic() +
  labs(x = 'Führungsstil') + 
  theme(legend.position="inside",
        legend.position.inside = c(0.2, 0.8)) + 
  coord_cartesian(ylim = c(5,15),
                  xlim = c(5,15)) 


## Michail
n = 36
group <- c(rep('error0', n), rep('error1', n), rep('error5', n))
id      = rep(as_factor(str_pad(1:n,2, pad="0")), 3)
df = as_tibble(data.frame(
  id    = id,
  group = group,
  trust = rnorm(n*3, 2.7, 0.7),
  ability = rnorm(n*3, 3, 0.9),
  benevolence = rnorm(n*3, 2.2, 0.8),
  integrity   = rnorm(n*3, 3, 0.7)))

# bar plot
ggplot(df, aes(x = group,
               y = trust)) +
  geom_bar(stat = "summary",
           width = 0.3,
           alpha = 0.4) +
  geom_jitter(size = 1.5,
           width = 0.075) +
  geom_linerange(stat = "summary",
           fun.data = "mean_se",
           fun.args = list(mult = 1), # 1 SE
           position = position_dodge(0.6),
           linewidth = 1,
           color = 'red') + 
  coord_cartesian(ylim = c(0,6)) +
  theme_classic() +
  labs(x = "Errors", y = "Score", title = "Trust") + 
  scale_x_discrete(labels= c("Zero", "One", "Five")) + 
  annotate("text", x = c(0.5),
                   y = c(5),
           label = c('p = .034'),
           vjust = 'bottom', hjust = 'left')  


# ANTONIA
n <- 40
A  <- rnorm(n/2, 0.6,0.1)
B <- rnorm(n/2, 0.8, 0.1)
df = as_tibble(data.frame(
  id       = as_factor(paste0("S", str_pad(1:n*2, 2, pad = "0"))),
  group    = as_factor(rep(rep(c("Individualgruppe", 
                                 "Kontrollgruppe"), each=n/2),2)),
  accuracy = c(A, B)))

ggplot(df, aes(x = group, 
               y = accuracy)) +
  geom_bar(stat = "summary",
           width = 0.5,
           position = position_dodge(0.6),
           alpha = 0.4) ... # see above

# JOHANNA
set.seed(12)
KG  <- rnorm(75, 25,4)
EG  <- rnorm(82, 25, 3.5)
df = as_tibble(data.frame(
     group = as_factor(c(rep("control", length(KG)),
                         rep("experimental", length(EG)))),
     self_esteem = c(KG, EG)))

library(ggdist) # for violin plots 

ggplot(df, aes(x = group,
               y = self_esteem)) +
  stat_halfeye(
    adjust = .5, 
    width = .4, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) +
  geom_jitter(size = 2,
              width = 0.02,
              color = 'gray80') +
  geom_pointrange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1.96), # i.e, 95%CI
                 linewidth = 1) +
  coord_cartesian(ylim = c(0,40)) +
  labs(x = "Group", y = "Self Esteem Score") + 
  theme_classic() 