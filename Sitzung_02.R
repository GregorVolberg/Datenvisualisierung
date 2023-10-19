library(tidyverse)
library(datasets)

ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point()

# oder Kurzform

ggplot(mpg, aes(displ, hwy)) +
  geom_point()


ggplot(mpg, aes(displ, hwy, color = fl)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, shape = fl)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, size = fl)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, alpha = fl)) +
  geom_point()

# line plot
ggplot(mpg, aes(x = (0:(length(year)-1))*20, y = year)) +
  geom_line()


ggplot(mpg, 
       aes(displ, hwy)) +
  geom_point()


ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy)) + 
  geom_abline(intercept = lm(mpg$hwy ~ mpg$displ)$coefficients[1], 
              slope = lm(mpg$hwy ~ mpg$displ)$coefficients[2])


