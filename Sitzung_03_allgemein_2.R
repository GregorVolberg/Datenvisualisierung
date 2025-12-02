library(tidyverse)

mm2inch <- function(x){x / 25.4}

dev.new(width = mm2inch(90),
        height = mm2inch(90*4/5),
        noRStudioGD=TRUE)

# Anm.: für das Datenbeispiel im Video brauchen Sie die Datei mit den Daten,
# "sub-S01_task-graspingMotorImagery_events.tsv" (in GRIPS).
# Alternativ könnnen Sie zum Ausprobieren auch einen Plot mit einem in R 
# integrierten Datensatz erstellen, zum Beispiel:
# ggplot(data = mpg, 
#       mapping = aes(x = displ,
#                     y = hwy)) +
#  geom_point(shape = 1) +
#  theme_classic(base_size = 10)

set.seed(21)
read_delim('./sub-S01_task-graspingMotorImagery_events.tsv',
           delim = '\t') %>%
  filter(movtype == 'imagined' & rating !='n/a') %>%
  #select(movement, rating) %>%
  mutate(Movement = as_factor(movement),
         Rating   = as.numeric(rating)) %>%
  ggplot(aes(x = Movement, y = Rating)) +
  geom_bar(stat = 'summary',
           width = 0.4,
           alpha = 0.6) +
  geom_jitter(width = 0.1) +
  coord_cartesian(ylim = c(1, 6)) +
  theme_classic(base_size = 10)
  
ggsave(filename = "motion.svg",
       width = mm2inch(90),
       height = mm2inch(90*4/5),
       units = 'in',
       dpi = 300)

