# WS25, Gruppe E

library(tidyverse)

data_nina <- function(){
  set.seed(5)
  n <- 40
  n_within_levels <- 6
  
  df <- data.frame(
    subject = rep(1:n, each = n_within_levels),
    group_type = rep(rep(c("kollab", "nomin"), each = n/2), each = n_within_levels),
    condition = rep(paste0("condition", 1:6), times = n),
    dv1 = runif(n * n_within_levels, min = 0.6, max = 0.7),
    dv2 = runif(n * n_within_levels, min = 0.1, max = 0.6)) %>%
    as_tibble() %>%
    mutate(across(subject:condition, ~as_factor(.)))  
}

data_anabel <- function(){
  df <- haven::read_spss('./Datensatz_Umfrage_Bachelorarbeit.sav') %>%
    mutate(Haustier = as_factor(Haustier_zusammen),
           lfdn = as_factor(lfdn)) %>%
    select(lfdn, Haustier, Depression_DASS, Allg.Lebenszufriedenheit_HSWBS,
           Satisfaction_with_Life_Scale)
  return(df)
}

data_madita <- function(){
  set.seed(5)
  
  n <- 192
  M  <- c(6.5, 3.2, 7.0)
  SD <- c(0.2, 0.4, 0.3)
  
  cor_matrix <- matrix(c(
    1.00,  0.267, 0.293,
    0.267,  1.00,  0.149,
    0.293,  0.149,  1.00
  ), nrow = 3, byrow = TRUE)
  
  cov_matrix <- diag(SD) %*% cor_matrix %*% diag(SD)
  
  df <- as_tibble((data.frame(ID = as_factor(1:n),
                        MASS::mvrnorm(n = n, 
                                mu = M, 
                                Sigma = cov_matrix,
                                empirical = TRUE)))) %>%
                  rename("A" = X1,
                         "B" = X2,
                         "C" = X3)
  return(df)
}

data_julian <- function(){
  set.seed(5)
  
  n <- 163
  M  <- c(4, 3)
  SD <- c(0.8, 0.5)
  
  cor_matrix <- matrix(c(
    1.00,  sqrt(0.023),
    sqrt(0.023),  1.00
  ), nrow = 2, byrow = TRUE)
  
  cov_matrix <- diag(SD) %*% cor_matrix %*% diag(SD)
  
  df <- as_tibble((data.frame(ID = as_factor(1:n),
                              MASS::mvrnorm(n = n, 
                                            mu = M, 
                                            Sigma = cov_matrix,
                                            empirical = TRUE)))) %>%
    rename("A" = X1,
           "B" = X2)
  return(df)
}

###### ===== Code ggplot
# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# bar graphs
df <- data_nina() %>%
      filter(condition %in% c("condition1", "condition2"))

ggplot(df, aes(x = group_type,
               y = dv1,
               fill  = condition,
               group = condition)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6),
           alpha = 0.4) +
  geom_jitter(aes(color = condition),
              size = 1.5,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  geom_linerange(stat = "summary",
                 fun.data = "mean_se",
                 fun.args = list(mult = 1), # 1 SE
                 position = position_dodge(0.6),
                 linewidth = 1,
                 color = 'black') +
# Konfektionierung  
  coord_cartesian(ylim = c(0,1)) +
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
  theme_classic() +
  labs(x = "Gruppe", y = "Hit Rate") +  
  scale_x_discrete(labels= c("kollaborativ", "nominal"))  + 
  #scale_x_discrete(labels= c("very long label A", "very long label B", "very long label C"),
  #                 guide = guide_axis(n.dodge = 2))  + 
  # theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="inside",
        legend.position.inside = c(0.2, 0.9),
        legend.title = element_blank()) +
  annotate("text", 
           x = 2, y = 0.8,
           label = 'p = .891',
           vjust = 'bottom', hjust = 'center')  

# Umsortieren von Faktorstufen
df2 <- df %>% 
        mutate(condition = fct_relevel(condition,
                                    "condition2",
                                   "condition1"))
ggplot(df2, aes(x = group_type,
               y = dv1,
               fill  = condition,
               group = condition)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6))

# Umbenennen von Faktorstufen
df3 <- df %>% 
  mutate(condition = fct_recode(condition,
                          neu1 =  "condition1",
                          neu2 =  "condition2"))
ggplot(df3, aes(x = group_type,
                y = dv1,
                fill  = condition,
                group = condition)) +
  geom_bar(stat = "summary",
           fun.data = "mean_se",
           width = 0.3,
           position = position_dodge(0.6))


#### Streudiagramme

df <- data_julian()

ggplot(df, aes(x = A,
               y = B)) +
  geom_smooth(method='lm', se = TRUE,
              level = 0.95,  
              fullrange = TRUE) + # 95% CI
  geom_point() + 
  # Konfektionierung
  theme_classic() +
  coord_cartesian(xlim = c(1,7), ylim = c(1, 7)) +
  scale_x_continuous(breaks = seq(1,7,1)) +
  scale_y_continuous(breaks = seq(1,7,1)) +
  labs(x = 'Free Will', y = 'Victim Blaming') 
  

