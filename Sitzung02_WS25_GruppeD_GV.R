library(tidyverse)
#library(haven)

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
  N <- 100
  df = as_tibble(c(data.frame(
    id = as_factor(str_pad(1:(N),2, pad="0")),
    factor_A = as_factor(c(rep('rp+', N/2), rep('rp-', N/2))),
    factor_B = as_factor(rep(c('0m', '4h', '24h', '7d'), N/4)),
    factor_C = as_factor(c(rep('nrp', N))),
    score    = runif(N, 40, 90))))
}

data_anushe <- function(){
  set.seed(5)
  N <- 20*3*4
    df = as_tibble(c(data.frame(
      id = as_factor(str_pad(1:(N),2, pad="0")),
      factor_A = as_factor(rep(c('A', 'B', 'C'),each = N/3)),
      factor_B = as_factor(rep(c('pre', 't1', 't2', 't3'), N/4)),
      AV1 = runif(N/3/4, 1, 3) + rep(c(0,1,2), each=N/3)))) 
}

data_marina <- function(){
  read_csv2('Froeber2025.csv') %>%
    pivot_longer(., SureTarget:SureLure, 
                 names_to = 'criterionValue', 
                 values_to = 'score') %>%
    pivot_wider(., names_from=type, values_from = score) %>%
    filter(criterionValue != "SureLure")
}
  
## Marina
data = data_marina()
ggplot(data, aes(x = lure, y = target, color = task)) + 
  geom_point() + 
  geom_line()



