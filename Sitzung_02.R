library(tidyverse)
library(datasets)
library(patchwork)
library(ggh4x)

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
   annotate("text", x=1.5, y=2, label = 'Group 1', color = 'red', 
            vjust = "middle", hjust = 'right') +
   annotate("text", x=-1, y=2, label = 'Group 2', color = 'blue',
            vjust = "middle", hjust = 'left') + 
   scale_x_continuous(limits = c(-3, 3),
                     breaks = seq(-3,3,1),
                     minor_breaks = seq(-3,3,0.5),
                     name = "Variable A",
                     guide = "axis_minor") + 
   scale_y_continuous(limits = c(-3, 3),
                     breaks = seq(-3,3,1),
                     minor_breaks = seq(-3,3,0.5),
                     name = "Variable B", 
                     guide = "axis_minor") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.ticks.length = unit(0.2, "cm"),
    ggh4x.axis.ticks.length.minor = rel(0.5))

# pseudo data
erp = tibble(wave1 = sin(seq(-pi,pi,0.1)),
             wave2 = sin(seq(pi,-pi,-0.1)),
             timebin = seq(1,length(wave1),1))
timeBinMs = 40
offset    = 21

ggplot() +
  geom_line(erp, mapping = aes((timebin-offset)*timeBinMs, wave2),
            color = 'blue') +
  geom_line(erp, mapping = aes((timebin-offset)*timeBinMs, wave1),
            color = 'red') +
  scale_x_continuous(breaks = c(-800,-400, 400, 800, 1200, 1600),
                     name = "Time (ms)") + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-0.5,0.5),
                     name = "microVolt") +
  theme_bw() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.ticks.length = unit(0.2, "cm"),
    axis.text  = element_text(size=12),
    axis.title = element_text(size=12),
    panel.border = element_blank()) +
    coord_axes_inside(xintercept = offset, yintercept=0, labels_inside = TRUE) +
    guides(x = guide_axis_truncated(trunc_lower = -800,
                       trunc_upper = 1600),
           y = guide_axis_truncated(trunc_lower = -0.5,
                                     trunc_upper = 0.5)) +
    annotate("rect", xmin = 300, xmax = 400, ymin = -0.5, ymax = 0.5,
           alpha = .1,fill = "blue") +
  annotate("text", x=-400, y=-0.5, label = 'Wave 1', color = 'red', 
           vjust = "middle", hjust = 'center') +
  annotate("text", x=-400, y=0.5, label = 'Wave 2', color = 'blue',
           vjust = "middle", hjust = 'center')
  
  



ggplot(erp, aes((timebin-offset)*timeBinMs, wave)) +
  geom_line() +
  scale_x_continuous(trans = (erp$timebin - 20)*40)
                     
  expand = c(0, 0)) 

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
  stat_smooth(g1, mapping = aes(a, b),method = lm, se = 1, color = 'blue') +
  labs(tag = "A")


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


