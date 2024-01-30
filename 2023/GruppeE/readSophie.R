library(tidyverse)
library(readxl)
#library(foreign)


filename <- "./2023/GruppeE/MLR_Sophie.xlsx"
raw <- read_xlsx(filename,
                 range = "E3:F62",
                 col_names = c("objektiv","vsro")) %>%
        bind_cols(
          read_xlsx(filename,
                    range = "E69:F128",
                    col_names = c("subKog",
                                  "vsrsk"))) %>%
        bind_cols(
          read_xlsx(filename,
                    range = "E133:F192",
                    col_names = c("subPhys",
                                  "vsrsp"))) %>%
    as_tibble()
  
# 
# filename <- "./2023/GruppeE/Sophie_VSR_diff.sav"
# 
# raw <- read.spss(filename) %>%
#   as_tibble() %>%
#   mutate(subject = as_factor(Subject),
#          gender = as_factor(str_sub(gender, 1, 1))) %>%
#   rename(pssa = starts_with("Physical"),
#          poimax = starts_with("Point"),
#          cgt1 = ends_with("cgt.1"),
#          cgt2 = ends_with("cgt.2"),
#          vsr1 = "VSR_diff.1",
#          vsr2 = "VSR_diff.2") %>%
#   select(subject, gender, age, pssa:vsr2)
         
