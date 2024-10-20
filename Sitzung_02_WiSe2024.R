# Sitzung 02, WiSe 2024

library(tidyverse)
library(haven) # is not loaded with tidyverse

id      <- paste0("id",str_pad(1:16, 2, pad = "0"))
study   <- as_factor(sample(c("BSc", "MSc"), 16, replace = TRUE))
pre_ws  <- round(rnorm(16, 520,20))
pst_ws  <- round(pre_ws + rnorm(16, 30,10))
pre_ss  <- round(pre_ws + rnorm(16, 10,3))
pst_ss  <- round(pst_ws - rnorm(16, 20,5))
df      <- as_tibble(data.frame(id, study, pre_ws, pst_ws, pre_ss, pst_ss))
write_sav(df, "example01.sav")


# ## Einlesen und modifizieren der Daten
# df1 <- read_sav("example01.sav") %>%
#         pivot_longer(
#           cols = pre_ws:pst_ss,
#           names_to = c("time", "semester"),
#           names_sep = "_",
#           values_to = "score") %>%       
#         mutate(across(id:semester, as_factor),
#                study = fct_recode(study,
#                           Master = "MSc",
#                           Bachelor = "BSc"),
#                study = fct_relevel(study,
#                                    "Bachelor",
#                                    "Master"))

df1 <- read_sav("exampleL07.sav") %>%
        pivot_longer(cols = c(pre, post1, post2),
                     names_to  = "time",
                     values_to = "RT") %>%
        mutate(time = as_factor(time),
               time = fct_recode(time,
                                 vorher = "pre",
                                 nachher = "post1",
                                 followup = "post2"),
               group = as_factor(group),
               group = fct_recode(group,
                                  Kontrolle = "cont",
                                  Treatment = "exp"),
               group = fct_relevel(group,
                                   "Treatment",
                                   "Kontrolle")) # Reihenfolge

# Farben
# use color-blind friendly palette, see 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")


# Bar- oder Strip-Plot
ggplot(df1, 
       mapping = aes(x = time, y = RT, fill = group)) +
  stat_summary(aes(group = group), 
               fun.data = "mean_se",
               fun.args = list(mult = 1), # 1 SE
               geom = "bar",
               width = 0.5,
               position = position_dodge(0.6),
               alpha = 0.4) + 
  scale_fill_manual(values = cbPalette) +
  geom_jitter(aes(color = group),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  stat_summary(aes(group = group),
               fun.data = "mean_se",
               fun.args = list(mult = 1), 
               geom = "linerange", # +- 1 SE around mean
               position = position_dodge(0.6),
               size = 1) + # size is width of errorbar
  theme_classic() +
  coord_cartesian(ylim = c(200,700),
                  xlim = c(0.7, 3.3)) +
  ylab("Reaktionszeit (ms)") +
  scale_x_discrete(labels = c("vorher", "nachher", "follow-up"),
                   name = "Zeitpunkt") 
  

# alternativ mit Linien
ggplot(df1, 
       mapping = aes(x = time, y = RT, fill = group)) +
  geom_jitter(aes(color = group),
              size = 2,
              position = position_jitterdodge(
                jitter.width = 0.2,
                dodge.width  = 0.6)) +
  scale_color_manual(values = cbPalette) +
  stat_summary(aes(group = group, color = group),
               fun.data = "mean_se",
               fun.args = list(mult = 1), 
               geom = "line", 
               position = position_dodge(0.6),
               size = 1) + # size is width of errorbar
  stat_summary(aes(group = group), 
               fun.data = "mean_se",
               fun.args = list(mult = 1), # 1 SE
               position = position_dodge(0.6),
               alpha = 0.4,
               size = 0.5,
               linewidth = 1) +
    theme_classic() +
  coord_cartesian(ylim = c(200,700),
                  xlim = c(0.7, 3.3)) +
  ylab("Reaktionszeit (ms)") +
  scale_x_discrete(labels = c("vorher", "nachher", "follow-up"),
                   name = "Zeitpunkt") 


# 2 x 2 x 2 plot


    

# Achsen limits entweder mit  coord_cartesian (besser)
coord_cartesian(ylim = c(0,700))

# oder mehr konfektinierung möglich mit scale_x_discrete etc (geht nicht mit stat_summary)
scale_x_discrete(labels = c("vorher", "nachher", "follow-up"),
                 name = "Zeitpunkt") +
scale_y_continous(limits = c(200, 700),
                  breaks = seq(200, 700, 50),
                  name = "Reaktionszeit (ms)")

# achsen- Labels mit xlab, ylab
xlab("Erhebungszeitpunkt") + 
  ylab("Reaktionszeit (ms)")

# kategoriale Achse mit scale_x_discrete
scale_x_discrete(labels=c("post1" = "Dose 0.5", "1" = "Dose 1",
                          "2" = "Dose 2"))

# color map ändern mit 
scale_color_manual() für aussenlinie
scale_fill_manual() für innen

# error bar mit stat_summary und geom = "errorbar"

  ggplot(df2, aes(x = time, y = RT, 
                fill = group)) + 
  geom_col(position = "dodge", stat = "summary", fun.y = "mean")
  
# abstand zwischen 2*2 plots entweder mit Paket patchwork (2 plots nebeneinander)
# alternative??


library(ggExtra) # contains ggMarginal
n = 60
df3 <- as_tibble(data.frame(var1 = rnorm(n),
                            var2 = rnorm(n),
                 gruppe = as_factor(rep(c("A","B"), n/2))))   

# df3 <- as_tibble(data.frame(var1 = sample(1:5,n, replace =T),
#                             var2 = rnorm(60),
#                             gruppe = as_factor(rep(c("A","B"),30))))

# for continuous x and y, use geom_point
plt1 <- ggplot(df3, aes(x = var1, y = var2, color = gruppe)) +
  geom_point() + # use for continuous x and y
  #geom_jitter(width = 0.05, height = 0) + #add some jitter to avoid overplotting
  scale_color_manual(values = cbPalette[2:3]) +
  geom_smooth(aes(group = gruppe), method='lm', se = TRUE, level = 0.95,
              fullrange = TRUE) + # 95% confidence interval
  theme_classic() +
  theme(legend.position="none") + 
  scale_x_continuous(expand = expansion(mult = 0.02), limits=c(-3, 3)) +
  scale_y_continuous(expand = expansion(mult = 0.02), limits=c(-3, 3))


plt2 <- ggMarginal(plt1, 
                   #type = "density", 
                   type = "histogram",
                   groupColour = TRUE,
                   groupFill = TRUE)
plt2

# xlim und ylim schauen
# jitter schauen
      
ggMarginal(piris, groupColour = TRUE, )