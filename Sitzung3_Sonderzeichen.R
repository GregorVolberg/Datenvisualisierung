library(tidyverse)

dat <- tibble(x = rnorm(30),
       y = x + rnorm(30,0,0.5))
coeff = lm(dat$y~dat$x)$coefficients

ggplot(dat, aes(x = x, y = y)) +
  geom_point() + 
  geom_abline(slope = coeff[2],
              intercept = coeff[1]) +
  theme_classic() +
  annotate("text", x = -1, y = 1,
          label = paste0("\u03B2 = ",
                         round(coeff[2], 3))) +
  theme(axis.title.y = 
          element_text(margin = margin(t = 0,
                                       r = 10,
                                       b = 0,
                                       l = 0)))

  
