library(tidyverse)
library(readxl)

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