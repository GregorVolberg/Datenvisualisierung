library(tidyverse)
library(foreign)

filename <- "./2023/GruppeE/Sarah_SPSS_alle_Daten.sav"

raw <- read.spss(filename) %>%
  as_tibble() %>%
  mutate(vp = as_factor(VP_Nummer)) %>%
  select(vp, alter = Alter, geschlecht = Geschlecht, 
         gruppe = GR01, wmt = WM_Gesamt, lgt = LGT3)

