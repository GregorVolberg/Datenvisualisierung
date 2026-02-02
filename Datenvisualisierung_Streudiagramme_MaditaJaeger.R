library(tidyverse)

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

df <- data_madita()


#Substreudiagramme mit den drei Korrelationen mit Referenzlinie und Fehlerbereich 

#Achsen richtig benennen und Subdiagramme erstellen, jedoch noch ohne gleichen Skalenbereich 

p1 <- ggplot(df, aes(x = A, y = B)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, fullrange = TRUE) +
  geom_point() +
  coord_cartesian(xlim = c(6.0, 7.0), ylim = c(2.0, 4.5)) +
  scale_x_continuous(breaks = seq(6.0, 7.0, 0.2)) +
  scale_y_continuous(breaks = seq(2.0, 4.5, 0.5)) +
  theme_classic() +
  labs(x = "Instagram-Aktivität", y = "Nutzungshäufigkeit")

p2 <- ggplot(df, aes(x = A, y = C)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, fullrange = TRUE) +
  geom_point() +
  coord_cartesian(xlim = c(6.0, 7.0), ylim = c(6.0, 8.0)) +
  scale_x_continuous(breaks = seq(6.0, 7.0, 0.2)) +
  scale_y_continuous(breaks = seq(6.0, 8.0, 0.5)) +
  theme_classic() +
  labs(x = "Instagram-Aktivität", y = "soziale Vergleichsorientierung")

p3 <- ggplot(df, aes(x = B, y = C)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, fullrange = TRUE) +
  geom_point() +
  coord_cartesian(xlim = c(2.0, 4.5), ylim = c(6.0, 8.0)) +
  scale_x_continuous(breaks = seq(2.0, 4.5, 0.5)) +
  scale_y_continuous(breaks = seq(6.0, 8.0, 0.5)) +
  theme_classic() +
  labs(x = "Nutzungshäufigkeit", y = "soziale Vergleichsorientierung")

library(patchwork)
p1 + p2 + p3

#mit gleichem Skalenbereich 

xlim = c(2, 8)
ylim = c(2, 8)

p1 <- ggplot(df, aes(x = A, y = B)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = TRUE,
              level = 0.95,
              fullrange = TRUE) +
  coord_cartesian(xlim = c(2, 8),
                  ylim = c(2, 8)) +
  scale_x_continuous(breaks = seq(2, 8, 1)) +
  scale_y_continuous(breaks = seq(2, 8, 1)) +
  theme_classic() +
  labs(
    x = "Instagram-Aktivität",
    y = "Nutzungshäufigkeit"
  )


p2 <- ggplot(df, aes(x = A, y = C)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = TRUE,
              level = 0.95,
              fullrange = TRUE) +
  coord_cartesian(xlim = c(2, 8),
                  ylim = c(2, 8)) +
  scale_x_continuous(breaks = seq(2, 8, 1)) +
  scale_y_continuous(breaks = seq(2, 8, 1)) +
  theme_classic() +
  labs(
    x = "Instagram-Aktivität",
    y = "soziale Vergleichsorientierung"
  )

p3 <- ggplot(df, aes(x = B, y = C)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = TRUE,
              level = 0.95,
              fullrange = TRUE) +
  coord_cartesian(xlim = c(2, 8),
                  ylim = c(2, 8)) +
  scale_x_continuous(breaks = seq(2, 8, 1)) +
  scale_y_continuous(breaks = seq(2, 8, 1)) +
  theme_classic() +
  labs(
    x = "Nutzungshäufigkeit",
    y = "soziale Vergleichsorientierung"
  )

library(patchwork)
p1 + p2 + p3





