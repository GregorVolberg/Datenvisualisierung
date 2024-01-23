library(tidyverse)
library(ggplot2)

### Daten Chiara
data <- read_delim('./2023/GruppeD/SemDiff_Chiara_Toscano.csv',
                   delim = ';') %>%
  group_by(pic_name) %>%
  summarize(across(freudig:ekelhaft, mean)) %>%
  pivot_longer(freudig:ekelhaft,
               names_to = "Emotion",
               values_to = "Mittelwert") %>%
  mutate(shape = as_factor(str_split_fixed(data$pic_name, "_", 3)[,2]))


ggplot(data, aes(x = Emotion, y = Mittelwert, color = shape)) +
  geom_point(position = position_jitter(width = 0.2), size = 1.5) + 
  geom_blank(aes(x = Inf, xend = -Inf, y = 0, yend = 0), size = 0.8) +  # Horizontale Achse
  geom_blank(aes(x = as.numeric(factor(Emotion)), xend = as.numeric(factor(Emotion)), y = Inf, yend = -Inf), size = 0.8) +  # Vertikale Achse
  labs(title = "Punktdiagramm der\nemotionalen Bewertung der Bilder",
       x = "Emotionen",
       y = "Mittelwerte der Bewertungshöhe",
       color = "Legende") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.line = element_line(color = "black", size = 0.5),  # Sichtbare Achsenlinien
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  # Keine Gitterlinien
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 20))) +
  scale_x_discrete(labels = c("ängstlich", "ekelhaft", "freudig", "traurig", "zornig")) +
  guides(color = guide_legend(title = "Form", title.theme = element_text(size = 10, hjust = 0.5))) +
  theme(legend.position = "right",
        plot.margin = margin(b = 0.1, unit = "cm")) +
  ylim(0.5, 5)  # Anpassung der Y-Achse
