library(tidyverse)
library(foreign)

#filename <- "D:/Bachelor/Sechstes Semester/Bachelorarbeit/Final.sav"
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

library(tidyverse)
library(patchwork)
library(ggplot2)

rt$condition = fct_recode(rt$condition,
                       hoch = "high",
                       sinkend = "decrease",
                       niedrig = "low",
                       steigend = "increase")

vsr$condition = fct_recode(vsr$condition,
                          hoch = "high",
                          sinkend = "decrease",
                          niedrig = "low",
                          steigend = "increase")
names(rt)[names(rt) == "condition"] <- "Belohnungssequenz"
names(vsr)[names(vsr) == "condition"] <- "Belohnungssequenz"

names(rt)[names(rt) == "rep_swtch"] <- "Aufgabenübergang"
rt$Aufgabenübergang = fct_recode(rt$Aufgabenübergang,
                           Aufgabenwiederholung = "repetition",
                           Aufgabenwechsel = "switch")


## Abbildung 1: Freiwillige Wechselrate 

plot_1 <- ggplot(data = vsr, 
       mapping = aes(x = block, y = vsr, color = Belohnungssequenz), stat = vsr)+
  stat_summary(geom = 'linerange',
               fun.data = "mean_se",
               position = position_dodge (0.2))+
  stat_summary(
    fun.data = "mean_se",
    geom = "pointrange",
    fun.args = list(mult = 1), 
    position = position_dodge(0.2))+
  labs(x = "Experimentalblock",
       y = "Freiwillige Wechselrate (in %)")+
  geom_line(stat = "summary",
            aes(group = Belohnungssequenz),
            fun.data = "mean_se",
            position = position_dodge(0.2),
            linetype = "solid")+
  theme_classic()
 
plot_1

install.packages("svglite")
library(svglite)
ggsave(filename = "Plot_1.svg",
       plot = plot_1,
       width = 5,
       units = 'cm',
       dpi = 300)
## Abbildung 2: Fehlerrate und Reaktionszeiten

plot_2 <- ggplot(data = rt, 
       mapping = aes(x = Belohnungssequenz, y = error, color = Aufgabenübergang), stat = rt)+
  stat_summary(geom = 'linerange',
               fun.data = "mean_se",
               position = position_dodge (0.2))+
  stat_summary(
    aes(group = Aufgabenübergang),
    fun.data = "mean_se",
    geom = "pointrange",
    fun.args = list(mult = 1), 
    position = position_dodge(0.2))+
  labs(x = "Belohnungssequenz",
       y = "Fehlerrate (in %)",
       tag = "A")+
  geom_line(stat = "summary",
            aes(group = Aufgabenübergang),
            fun.data = "mean_se",
            position = position_dodge(0.2),
            linetype = "solid")+
  theme_classic()

plot_3 <- ggplot(data = rt, 
       mapping = aes(x = Belohnungssequenz, y = rt, color = Aufgabenübergang), stat = rt)+
  stat_summary(geom = 'linerange',
               fun.data = "mean_se",
               position = position_dodge (0.2))+
  stat_summary(
    aes(group = Aufgabenübergang),
    fun.data = "mean_se",
    geom = "pointrange",
    fun.args = list(mult = 1), 
    position = position_dodge(0.2))+
  labs(x = "Belohnungssequenz",
       y = "Reaktionszeiten (in ms)",
       tag = "B")+
  geom_line(stat = "summary",
            aes(group = Aufgabenübergang),
            fun.data = "mean_se",
            position = position_dodge(0.2),
            linetype = "solid")+
  theme_classic()

# Arrangieren von Plot 2 & 3

plot_2 | plot_3
