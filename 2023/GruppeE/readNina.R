library(tidyverse)
library(foreign)

filename <- "./2023/GruppeE/Final_Nina.sav"

raw <- read.spss(filename) %>%
  as_tibble() %>%
  select(age, sex,ID = openLabId,
         VSRrlow1:VSRdec4,
         Erlowr1:Edecs4,
         RTrlowr1:RTdecs4) %>%
  mutate(ID = as_factor(str_replace(ID,
                                    "          ",
                                    "abcdefghij")))
#======= vsr
vsr <- raw %>% select(age, sex,ID,
               VSRrlow1:VSRdec4) %>%
        pivot_longer(
          cols = VSRrlow1:VSRdec4,
          names_to = "tmp",
          values_to = "vsr") %>%
        mutate(block = as_factor(str_sub(tmp, -1)),
             condition = as_factor(str_sub(tmp, -4, -2)),
             condition = fct_recode(condition,
                                    high = "igh",
                                    decrease = "dec",
                                    low = "low",
                                    increase = "inc"),
             sex = as_factor(sex)) %>%
        select(-tmp)

#===== RT and errors (collapsed across blocks)
rt <- raw %>% select(age, sex,ID,
                      Erlowr1:RTdecs4) %>%
  pivot_longer(
    cols = Erlowr1:RTdecs4,
    names_to = "tmp",
    values_to = "tmpval") %>%
  mutate(block = as_factor(str_sub(tmp, -1)),
         condition = as_factor(str_sub(tmp, -5, -3)),
         condition = fct_recode(condition,
                                high = "igh",
                                decrease = "dec",
                                low = "low",
                                increase = "inc"),
         ert = str_sub(tmp, 1, 1),
         rs  = as_factor(str_sub(tmp, -2, -2)),
         rep_swtch = fct_recode(rs, repetition = "r", 
                                 switch = "s"),
         sex = as_factor(sex)) %>%
  select(-tmp, -rs) %>%
  pivot_wider(names_from = ert, values_from = tmpval) %>%
  group_by(ID, condition, rep_swtch) %>%
  summarise(error = mean(E, na.rm = T),
            rt = mean(R, na.rm = T)) %>%
  ungroup()
