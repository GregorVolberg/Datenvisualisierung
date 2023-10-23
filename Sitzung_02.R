library(tidyverse)
library(datasets)

ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point()

# oder Kurzform

ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# Drucksymble
ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(displ, hwy, color = displ)) +
  geom_point()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(color = 'blue')


# 
ggplot(mpg, aes(displ, hwy)) +
  geom_point(shape = 8,
             color = 'red',
             size = 4)


ggplot(mpg, aes(displ, hwy)) +
  geom_point(shape = 8,
             color = 'red',
             size = 4)

ggplot(mpg, aes(year, cyl, group = class)) +
  geom_point()

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


plt <- ggplot(audi, aes(model, m,
                 ymin = m - se,
                 ymax = m + se))

plt + 
  geom_point() +
  geom_linerange()
  

plt + 
  geom_point() +
  geom_linerange() +
  labs(
    x = "Model",
    y = "Mean mileage on highways",
    title = "Mileage by model"
  )

ggplot(mpg, aes(displ, hwy)) +
    geom_point() + 
    scale_x_continuous(limits = c(0, 10),
                       breaks = seq(0,10,2),
                       minor_breaks = seq(0,10,0.5),
                       labels = seq(-2,8,2),
                       name = "Displacement") + 
    scale_y_continuous(limits = c(0, 45),
                       breaks = seq(0,50,10),
                       minor_breaks = seq(0,45,5),
                       name = "Mileage") 

ausgangsplot <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  scale_x_continuous(limits = c(0, 10),
                     breaks = seq(0,10,2),
                     minor_breaks = seq(0,10,0.5),
                     labels = seq(-2,8,2),
                     name = "Displacement") + 
  scale_y_continuous(limits = c(0, 45),
                     breaks = seq(0,50,10),
                     minor_breaks = seq(0,45,5),
                     name = "Mileage")

ausgangsplot +
  theme_bw() +
  theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  geom_hline(20)
  

# plot 1: scatter plot
a = rnorm(100); 
b = a + rnorm(100,0, 0.5);
b[51:100] = b[51:100]* -1
grp = c(rep('g1', 50), rep('g2', 50))
dat = tibble(a=a, b=b, grp = grp)

ggplot(dat, mapping = aes(a, b, color = grp)) +
   geom_point() +
   stat_smooth(dat, mapping = aes(a, b, group = grp), method = lm, se = 1) +
   scale_colour_manual(values=c("red", "blue")) +
   theme(legend.position = "none") +
   annotate("text", x=1,y=2, label = 'Group 1', color = 'red') +
   annotate("text", x=-1,y=2, label = 'Group 2', color = 'blue')

  

g1 <- dat %>% filter(grp == 1)
g2 <- dat %>% filter(grp == 2)
ggplot() +
  geom_point(g1, mapping = aes(a, b), color = 'blue') +
  stat_smooth(g1, mapping = aes(a, b),method = lm, se = 1, color = 'blue') +
  geom_point(g2, mapping = aes(a, b), color = 'red') +
  stat_smooth(g2, mapping = aes(a, b),method = lm, se = 1, color = 'red')

library(patchwork)  
  
plt1 <- ggplot() +
  geom_point(g1, mapping = aes(a, b), color = 'blue') +
  stat_smooth(g1, mapping = aes(a, b),method = lm, se = 1, color = 'blue')

plt1 | plt1 / plt1



plt + 
  geom_point() +
  geom_linerange() + 
  scale_x_discrete(limits = c("a4 quattro",
                              "a6 quattro",
                              "a4"),
                   guide = guide_axis(angle = 90))
  
  


ggplot(mpg, aes(displ, hwy, group = class)) +
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


