source('./2023/GruppeE/readSophie.r')
library(patchwork)

# difference score and residuals
dat <- raw %>%
  mutate(vsrdiff   = (vsr1 - vsr2) * 100,
         pssares   = lm(vsrdiff ~ poimax + cgt1)$residuals,
         poimaxres = lm(vsrdiff ~ pssa + cgt1)$residuals,
         cgt1res   = lm(vsrdiff ~ pssa + poimax)$residuals)

# 4a
plt1 <- dat %>% 
  mutate(x = poimax, y = poimaxres) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  theme_classic() +
  labs(y = "Difference voluntary switch rate (%)",
       x = "Maximal res (?)",
       tag = "A")

# 5a
plt2 <- dat %>% 
  mutate(x = pssa, y = pssares) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  theme_classic() +
  labs(y = "Difference voluntary switch rate (%)",
       x = "pssa (?)",
       tag = "B") 

# 6a
plt3 <- dat %>% 
  mutate(x = cgt1, y = cgt1res) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  theme_classic() +
  labs(y = "Difference voluntary switch rate (%)",
       x = "pssa (?)",
       tag = "C") 

plt1 + plt2 + plt3