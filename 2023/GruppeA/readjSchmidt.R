library(tidyverse)
library(readxl)

raw <- read_excel('jSchmidt.xlsx') %>%
  filter(!(Note_Deutsch == "-")) %>%
  mutate(across(c(AFS_6, AFS_21, AFS_46), ~ abs(.x -1))) %>% # recode
  mutate(across(Note_Deutsch:Note_HSU, as.numeric)) %>%      
  select(AFS_6,  AFS_10, AFS_16, AFS_17, AFS_21,
         AFS_29, AFS_31, AFS_35, AFS_46, AFS_50,
         Note_Deutsch, Note_Mathe, Note_HSU) %>%
  drop_na() %>%
  rowwise   %>% 
  mutate(SU = sum(across(AFS_6:AFS_50), na.rm=TRUE)) %>%
  ungroup   %>% 
  select(SU, Note_Deutsch, Note_Mathe, Note_HSU)

cor.test(raw$SU, raw$Note_Deutsch)
cor.test(raw$SU, raw$Note_Mathe)
cor.test(raw$SU, raw$Note_HSU)

write_csv(file = 'rawdataJS.csv', raw)
dat <- read_csv(file = 'rawdataJS.csv')