source('./2023/GruppeE/readSophie.r')
library(patchwork)

# # difference score and residuals
# dat <- raw %>%
#   mutate(vsrdiff   = (vsr1 - vsr2) * 100,
#          pssares   = lm(vsrdiff ~ poimax + cgt1)$residuals,
#          poimaxres = lm(vsrdiff ~ pssa + cgt1)$residuals,
#          cgt1res   = lm(vsrdiff ~ pssa + poimax)$residuals)

# 4a
plt1 <- raw %>% 
  ggplot(aes(x = objektiv, y = vsro)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  theme_classic() +
  labs(y = "Difference voluntary switch rate (%)",
       x = "Objektiv",
       tag = "A")

# 5a
plt2 <- raw %>% 
  ggplot(aes(x = subKog, y = vsrsk)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  theme_classic() +
  labs(y = "Difference voluntary switch rate (%)",
       x = "Subjektiv-kognitiv",
       tag = "B") 

# 6a
plt3 <- raw %>% 
  ggplot(aes(x = subPhys, y = vsrsp)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray') +
  theme_classic() +
  labs(y = "Difference voluntary switch rate (%)",
       x = "Subjektiv-physisch",
       tag = "C") 

plt1 + plt2 + plt3