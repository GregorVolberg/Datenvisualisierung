library(tidyverse)
library(foreign)

filename <- "./2023/GruppeB/Vergleich RT Vor_Nachtest ohne Ausreißer über 3.sav"

raw <- read.spss(filename) %>%
  as_tibble() %>%
  rename("n_sessions" = vp_nr) %>%
  mutate(vp = as_factor(str_c("vp", 1:nrow(.)))) %>%
  pivot_longer(cols = c(Vortest_PRL:Nachtest_OPP),
               names_to  = "prepost",
               values_to = "val") %>%
  separate(prepost, into = c("prepost", "av")) %>%
  pivot_wider(names_from = av, values_from = val) %>%
  mutate(n_sessions = fct_recode(n_sessions, 
                        vier = "vier sitzungen",
                        acht = "acht sitzungen"),
         prepost = fct_recode(prepost,
                        pre = "Vortest",
                        post = "Nachtest")) %>%
  select(vp, n_sessions, prepost, PRL, OPP) %>%
  write_csv(file = "./2023/GruppeB/Julia.csv")

#dat <- read_csv(file = './2023/GruppeB/Julia.csv')