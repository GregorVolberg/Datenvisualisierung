library(tidyverse)

mm2inch <- function(x){x / 25.4}

dev.new(width = mm2inch(90),
        height = mm2inch(90*4/5),
        noRStudioGD=TRUE)

ggplot(data = mpg, 
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_point(shape = 1) +
  theme_classic(base_size = 10)

ggsave(filename = "plot1.svg",
       width = mm2inch(90),
       height = mm2inch(90*4/5),
       units = 'in',
       dpi = 300)


# for maintaining size in R graphics devices, see
# see https://stackoverflow.com/questions/44711236/set-the-size-of-ggsave-exactly

# for text modifications in Inkscape see
# https://graphicdesign.stackexchange.com/questions/160105/why-changing-font-size-only-changes-height
