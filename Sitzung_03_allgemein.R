library(tidyverse)

ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point(shape = 1) +
  theme_classic()


g1 <- ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point(shape = 1) +
  theme_classic()

ggsave(plot = g1,
       filename = "myPlot2.svg",
       width = 391,
       height = 362,
       units = 'px',
       dpi = 300)

# for maintaining size, see
# see https://stackoverflow.com/questions/44711236/set-the-size-of-ggsave-exactly


# text modificarions see
# https://graphicdesign.stackexchange.com/questions/160105/why-changing-font-size-only-changes-height
# # Kurzform
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point()
# 
# # mapping Drittvariable
# ggplot(mpg, aes(displ, hwy,
#                 color = class)) +
#   geom_point()
# 
# 
# # Konfektionierung Daten
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point(shape = 8,
#              color = 'red',
#              size = 4)
# 
# 
# # Konfektionierung Daten: shape
# ggplot(mpg, aes(displ, hwy,
#                 shape = class)) +
#   geom_point() +
#   scale_shape_manual(values = 1:7)
# 
# 
# ### Achsen
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() + 
#   coord_cartesian(xlim = c(0,10), ylim = c(0, 45))+
#   scale_x_continuous(breaks = c(2,6),
#                      minor_breaks = seq(0,10,0.5),
#                      labels = c("wenig", "viel"),
#                      name = "Displacement") + 
#   scale_y_continuous(breaks = seq(0,50,10),
#                      minor_breaks = seq(0,45,5),
#                      name = "Mileage") 
# 
# # Anmerkungen
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   geom_hline(yintercept = 35,
#              linetype = 'dashed') +
#   annotate("rect", xmin = 1, xmax = 2,
#            ymin = 20, ymax = 50,
#            alpha = .1,fill = "blue") +
#   annotate("text", x = 2.5, y = 40,
#            label = 'Economic cars',
#            vjust = "center", hjust = "left")
# 
# # themes
# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   coord_cartesian(xlim = c(0,10), ylim = c(0, 45))+
#   theme_classic() +
#   theme(
#     axis.ticks.length = unit(0.2, "cm"),
#     axis.ticks.x = element_line(color = 'red'))
# 
# # Objekt
# myPlot <- ggplot(mpg, aes(displ, hwy)) 
# myPlot + geom_point()
# 
# # export
# myPlot <- ggplot(data = mpg, 
#                  mapping = aes(x = displ,
#                                y = hwy)) +
#   scale_x_continuous(name = "Displacement") + 
#   scale_y_continuous(name = "Mileage") +
#   theme_classic() +
#   geom_point()
# 
# # ggfs. Paket svglite installieren
# 
