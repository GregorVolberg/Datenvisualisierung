
library(tidyverse)
library(readxl)
library(ggplot2)

data_julian <- function(){
  df <- readxl::read_xlsx('DatensatzVollständig.xlsx') %>%
    filter(row.names(.) != "1") %>%
    mutate(across(starts_with("FW01"), as.numeric),
           across(starts_with("VB"), as.numeric)) %>%
    mutate(FW = rowMeans(select(., starts_with('FW')), na.rm=T),
           VB = rowMeans(select(., starts_with('VB')), na.rm=T))
  return(df)
}
df <- data_julian()

df <- df %>%
  mutate(
    # Geschlecht rekodieren
    Geschlecht = factor(
      DE01,
      levels = c(1, 2, 3),
      labels = c("weiblich", "männlich", "divers")
    ),
    
    # Altersgruppe bilden
    Altersgruppe = ifelse(DE02_01 < 40, "< 40", "≥ 40"),
    Altersgruppe = factor(Altersgruppe)
  )


df_plot <- df %>%
  filter(
    !is.na(Geschlecht),
    !is.na(Altersgruppe),
    between(FW, 1, 7),
    between(VB, 1, 7)
  )

ggplot(df_plot, aes(
  x = FW,
  y = VB,
  color = Geschlecht,
  shape = Altersgruppe
)) +
  geom_point(size = 2.5, alpha = 0.8) +
  

  geom_smooth(
    data = df_plot,
    aes(x = FW, y = VB),
    method = "lm",
    se = TRUE,
    level = 0.95,
    color = "black",
    fill = "grey70",
    inherit.aes = FALSE
  ) +
  
  theme_classic() +
  coord_cartesian(xlim = c(1,7), ylim = c(1,7)) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7) +
  scale_shape_manual(values = c("< 40" = 16, "≥ 40" = 15)) +
  labs(
    x = "Free Will",
    y = "Victim Blaming",
    color = "Geschlecht",
    shape = "Alter"
  )


