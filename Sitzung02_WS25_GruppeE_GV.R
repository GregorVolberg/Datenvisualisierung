# WS25, Gruppe E
############============= Claude
library(tidyverse)
library(MASS) # for generating the multivariate normal distribution

set.seed(5)

n <- 192
M  <- c(6.5, 3.2, 7.0)
SD <- c(0.2, 0.4, 0.3)

cor_matrix <- matrix(c(
  1.00,  0.23, -0.70,
  0.23,  1.00,  0.01,
  -0.70,  0.01,  1.00
), nrow = 3, byrow = TRUE)

cov_matrix <- diag(SD) %*% cor_matrix %*% diag(SD)

df <- as_tibble((data.frame(ID = as_factor(1:n),
                      mvrnorm(n = n, 
                              mu = M, 
                              Sigma = cov_matrix,
                              empirical = TRUE)))) %>%
                rename("A" = X1,
                       "B" = X2,
                       "C" = X3)


##############================= claude end
###############======== again claude 
# Set seed for reproducibility
set.seed(123)

# Design parameters
n <- 30  # number of participants
levels_A <- 4
levels_B <- 2
levels_C <- 3

# Create participant IDs
participant_ID <- rep(1:n, each = levels_A * levels_B * levels_C)

# Create all combinations of factors for each participant
A <- rep(rep(1:levels_A, each = levels_B * levels_C), times = n)
B <- rep(rep(1:levels_B, each = levels_C), times = n * levels_A)
C <- rep(1:levels_C, times = n * levels_A * levels_B)

# Convert to factors with labels
A <- factor(A, labels = paste0("A", 1:levels_A))
B <- factor(B, labels = paste0("B", 1:levels_B))
C <- factor(C, labels = paste0("C", 1:levels_C))

# Generate dependent variable X (with some random variation)
X <- rnorm(n * levels_A * levels_B * levels_C, mean = 50, sd = 10)

# Create data frame
df <- data.frame(
  Subject = participant_ID,
  A = A,
  B = B,
  C = C,
  X = X
)
#################=============== end claude


library(tidyverse)

# functions for data generation
generate_data_paula <- function(){
  set.seed(5)
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
}

generate_data_paul <- function(){
  set.seed(13)
  N <- 40
  sigma  <- rbind(c(1,0.107, -0.602), c(0.107, 1, 0.193), c(-0.602,0.193,1)) # correlation matrix
  mu     <- c(5,3,6) # mean of variables
  corrmat <- rbind(MASS::mvrnorm(n = N, mu = mu,
                                 Sigma = sigma,
                                 empirical = TRUE))
    df = as_tibble(data.frame(
    id      = as_factor(paste0("S", str_pad(1:n, 2, pad = "0"))),
    corrmat)) %>%
    rename(KI = 'X1', sozEingeb = 'X2', traitAngst = 'X3')
}

generate_data_michail <- function(){
  set.seed(17)
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
}

generate_data_astrid <- function(){
  set.seed(19)
  n  <- 46
  id        <- as_factor(1:n)
  trial     <- as_factor(1:5)
  condition <- as_factor(1:4)
  df = as_tibble(data.frame(
    id      = rep(id, each = length(trial) * length(condition)),
    trial   = rep(trial, length(condition)),
    condition = rep(condition, each = length(trial)),
    bis_error = rnorm(n * length(trial) * length(condition))))
}


# color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

# PAULA
df <- generate_data_paula()
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
        legend.position.inside = c(0.15, 0.85)) + 
  coord_cartesian(ylim = c(6.5,13),
                  xlim = c(6.5, 13)) 

# Paul
df <- generate_data_paul()
ggplot(df, aes(x = traitAngst, 
               y = sozEingeb,
               color = cut_interval(KI, 3,
                      labels = c('low',
                      'medium', 'high')))) +
  geom_smooth(method='lm', se = TRUE,
              level = 0.95,  
              fullrange = TRUE) + # 95% CI
  geom_point() + 
  scale_fill_manual(values = cbPalette)  +
  scale_color_manual(values = cbPalette) +
  theme_classic() +
  labs(x = 'Trait-Angst', y = 'Soziale Eingebundenheit',
       color = 'KI') + 
  theme(legend.position="inside",
        legend.position.inside = c(0.15, 0.85)) + 
  coord_cartesian(ylim = c(0,8),
                  xlim = c(4,8)) 


# Michail
df <- generate_data_michail()
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

# Astrid

library(ggExtra)
df <- generate_data_astrid()
plot1 <- df %>% 
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

ggMarginal(plot1, groupColour = TRUE, groupFill = TRUE, margins = "x")

# for patchwork, use with patchwork::wrap_elements
library(patchwork)
gplot1 <- ggMarginal(plot1, groupColour = TRUE,
           groupFill = TRUE, margins = "x")
wrap_elements(gplot1) | wrap_elements(gplot1) 
