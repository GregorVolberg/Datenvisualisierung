library(tidyverse)
library(patchwork)

ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point()

# Kurzform
ggplot(mpg, aes(displ, hwy)) +
  geom_point()

### mapping Drittvariable
ggplot(mpg, aes(displ, hwy,
                color = class)) +
  geom_point()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

### Objekt
myPlot <- ggplot(mpg, aes(displ, hwy)) 
myPlot + geom_point()

### geoms
ggplot(mpg, aes(class)) +
  geom_bar() 

ggplot(mpg, aes(x = cyl, y = displ)) +
    stat_summary(geom = 'pointrange',
                 fun.data = "mean_se")

# geom_smooth
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm", 
              se = 1,
              color = 'darkgray')


# geom_jitter
ggplot(mpg, aes(x = cyl, y = displ)) +
  geom_jitter(width = 0.05,
              color = 'darkgrey') + 
  stat_summary(geom = 'pointrange',
               fun.data = 'mean_se')


# line plot Mittelwerte
mpg <- mpg %>% mutate(schalt = 
                 case_when(
                   str_detect(trans,'auto') ~ 'automatic',
                   .default = 'manual'),
               class2 = 
                 case_when(
                   str_detect(class,'compact') ~ 'compact',
                   .default = 'other'))

ggplot(mpg, aes(class2, hwy)) +
  stat_summary(
    aes(group = schalt, color = schalt),
    fun.data = "mean_se",
    geom = "line", 
    position = position_dodge(0.2)) +
  stat_summary(
    aes(group = schalt),
    fun.data = "mean_se",
    geom = "pointrange",
    fun.args = list(mult = 1), 
    position = position_dodge(0.2))



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


### shape maps, color maps
ggplot(mpg, aes(displ, hwy,
                shape = class)) +
  geom_point() +
  scale_shape_manual(values = 1:7)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_color_manual(values = cbPalette)


### Achsen
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  coord_cartesian(xlim = c(0,8),
                  ylim = c(0, 80)) +
  labs(x = "Hubraum",
             y = "Reichweite",
             tag = "A")


### themes
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  theme_classic()


# export
myPlot <- ggplot(mpg, 
                 aes(displ, hwy)) +
  geom_point() + 
  theme_classic()

# arrangieren
plot1 <- myPlot + labs(tag = "A")
plot2 <- myPlot + labs(tag = "B")

plot1 | plot2
plot1 | plot2 / plot1


ggsave(filename = "myPlot.svg", 
       plot = myPlot,
       width = 5,
       height = 5,
       units = 'cm',
       dpi = 300)

