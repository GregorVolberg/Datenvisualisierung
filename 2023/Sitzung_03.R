library(tidyverse)

# low data-ink ratio
ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  scale_x_continuous(limits = c(0, 10),
                     minor_breaks = seq(0,10,0.5),
                     name = "Displacement") + 
  scale_y_continuous(limits = c(0, 45),
                     breaks = seq(0,50,10),
                     minor_breaks = seq(0,45,5),
                     name = "Mileage") +
  geom_point()

# high data-ink ratio
myPlot <- ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  scale_x_continuous(limits = c(0, 8),
                     name = "Displacement") + 
  scale_y_continuous(limits = c(5, 45),
                     name = "Mileage") +
  theme_classic() +
  geom_point()

# speichern
ggsave(filename = "myPlot.svg", 
       plot = myPlot,
       width = 5,
       height = 5,
       units = 'cm',
       dpi = 300)




# Kurzform
ggplot(mpg, aes(displ, hwy)) +
  geom_point()

### Objekt
myPlot <- ggplot(mpg, aes(displ, hwy)) 
myPlot + geom_point()


### mapping Drittvariable
ggplot(mpg, aes(displ, hwy,
                color = class)) +
  geom_point()

### alle Drucksymbole
ggplot(mpg, aes(displ, hwy)) +
  geom_point(color = 'blue')

### geoms
ggplot(mpg, aes(class)) +
  geom_bar() 
  
### Streuungsmaße
audi <- mpg %>%
  filter(manufacturer == "audi") %>%
  group_by(model) %>%
  summarize(m = mean(hwy),
            se = sd(hwy)/sqrt(n()))

ggplot(audi, aes(model, m,
                 ymin = m - se,
                 ymax = m + se)) +
  geom_point() +
  geom_linerange()


### Anmerkungen
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_hline(yintercept = 35,
             linetype = 'dashed') +
  annotate("rect", xmin = 1, xmax = 2,
           ymin = 20, ymax = 50,
           alpha = .1,fill = "blue") +
  annotate("text", x = 2.5, y = 40,
           label = 'Economic cars',
           vjust = "center", hjust = "left")


### Darstellungsoptionen 
ggplot(mpg, aes(displ, hwy)) +
  geom_point(shape = 8,
             color = 'red',
             size = 4)

### shape maps
ggplot(mpg, aes(displ, hwy,
                shape = class)) +
  geom_point() +
  scale_shape_manual(values = 1:7)

### Achsen
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(2,6),
                     minor_breaks = seq(0,10,0.5),
                     labels = c("wenig", "viel"),
                     name = "Displacement") + 
  scale_y_continuous(limits = c(0, 45),
                     breaks = seq(0,50,10),
                     minor_breaks = seq(0,45,5),
                     name = "Mileage") 

### themes
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(limits = c(0, 10),
     breaks = seq(0,10,2),
     minor_breaks = seq(0,10,1),
     guide = "axis_minor") + 
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0,50,10),
    minor_breaks = seq(5,45,10),
    guide = "axis_minor") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.ticks.length = unit(0.25, "cm"),
    ggh4x.axis.ticks.length.minor = rel(0.5))

