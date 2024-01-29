source('./2023/GruppeE/readSophie.r')

# difference score and residuals
dat <- raw %>%
  mutate(vsrdiff   = (vsr1 - vsr2) * 100,
         pssares   = lm(vsrdiff ~ poimax + cgt1)$residuals,
         poimaxres = lm(vsrdiff ~ pssa + cgt1)$residuals,
         cgt1res   = lm(vsrdiff ~ pssa + poimax)$residuals)

# 4a
lmres <- lm(poimaxres ~ poimax, data = dat)
dat %>% ggplot(aes(x = poimax, y = poimaxres)) + 
  geom_point() +
  geom_abline(slope     = lmres$coefficients[1],
              intercept = lmres$coefficients[2],
              ty) +
  labs(x = "poimaxres",
       y = "Difference voluntary switch rate (%)") +
  theme_classic()

  ggplot(aes(x = poimaxres, y = vsrdiff)) + 