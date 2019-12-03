### title: Line graph with error bars ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 09/30/2019
### Description: Abundancia de grupos em cada parcela e em cada amostragem por tratamento em um grafico linear com desvio padrao

rm(list = ls()) 

library(tidyverse)
library(boot)

bt.dt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
bt.dt 

bt.dt.slc <- bt.dt [,-c(1,5,23:28)]
bt.dt.slc

bt.dt.slc.gtr <- bt.dt.slc %>% 
  gather(key = "time", value = "value", 4:20)
bt.dt.slc.gtr


### Palmeiras

palmeiras <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "palm" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
palmeiras


#palmeiras$Treatment <- as.factor(palmeiras$Treatment)
#palmeiras
#palmeiras$time <- as.factor(palmeiras$time)
#palmeiras


### grafico de linhas com erro padrão - Modo 1


palm <- ggplot(palmeiras, aes(time, abundancia, color = Treatment)) +
        scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
        stat_summary(fun.data = mean_cl_boot,
                     geom = "errorbar",
                     width = 0.2,
                     aes(group = Treatment),
                     color = "black",
                     fun.args = list(conf.int = .95, B = 2000)) +
        stat_summary(fun.y = mean,
                     geom = "point",
                     size = 3,
                     aes(shape = Treatment),
                     show.legend = FALSE) +
        stat_summary(fun.y = mean, 
                     geom = "line",
                     aes(group = Treatment),
                     size = 1) +
        theme(axis.title = element_text(size = 12, face = "bold"), legend.position = "none") +
        labs(x = "Sampled period (months)", y = "Total palms abundances  (log)") +
  annotate("text", label = 'atop(bold("B"))', parse= TRUE, size = 7, x = 17, y = 2.90) +
         #    annotate("text", label = "2009", size = 3, x = 1, y = 1.866) +
          #   annotate("text", label = "2010", size = 3, x = 3, y = 1.71) +
           #  annotate("text", label = "2011", size = 3, x = 5, y = 1.789) +
            # annotate("text", label = "2012", size = 3, x = 7, y = 1.97) +
        #     annotate("text", label = "2013", size = 3, x = 9, y = 1.68) +
         #    annotate("text", label = "2014", size = 3, x = 11, y = 1.71) +
          #   annotate("text", label = "2015", size = 3, x = 13, y = 1.81) +
           #  annotate("text", label = "2016", size = 3, x = 15, y = 1.71) +
            # annotate("text", label = "2017", size = 3, x = 17, y = 1.735) +
        scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96")) +
        expand_limits(y=3)
palm
ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/palms/error_bar_line_palm_total.png", w = 20, h = 10, units = "cm", dpi = 300)

## por area


# CBO

palmeiras.cbo <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CBO", `Life Form` == "palm" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
palmeiras.cbo


palm.cbo <- ggplot(palmeiras.cbo, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Palms abundances (CBO)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
palm.cbo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/palms/error_bar_line_palm_cbo.png", w = 20, h = 10, units = "cm", dpi = 300)


# VGM

palmeiras.vgm <- bt.dt.slc.gtr %>% 
  filter(`Site` == "VGM", `Life Form` == "palm" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
palmeiras.vgm


palm.vgm <- ggplot(palmeiras.vgm, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Palms abundances (VGM)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
palm.vgm

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/palms/error_bar_line_palm_vgm.png", w = 20, h = 10, units = "cm", dpi = 300)


# ITA


palmeiras.ita <- bt.dt.slc.gtr %>% 
  filter(`Site` == "ITA", `Life Form` == "palm" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
palmeiras.ita


palm.ita <- ggplot(palmeiras.ita, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Palms abundances (ITA)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
palm.ita

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/palms/error_bar_line_palm_ita.png", w = 20, h = 10, units = "cm", dpi = 300)



# CAR

palmeiras.car <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CAR", `Life Form` == "palm" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
palmeiras.car


palm.car <- ggplot(palmeiras.car, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Palms abundances (CAR)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
palm.car

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/palms/error_bar_line_palm_car.png", w = 20, h = 10, units = "cm", dpi = 300)





### Arvores

arvores <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "tree" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arvores


tree <- ggplot(arvores, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Total trees abundances (log)") +
  theme(axis.title = element_text(size = 12, face = "bold"),
    legend.position = c(0.12, .90)) +
  annotate("text", label = "2009", size = 2.5, x = 1, y = 2.29) +
  annotate("text", label = "2010", size = 2.5, x = 3, y = 2.399) +
  annotate("text", label = "2011", size = 2.5, x = 5, y = 2.4) +
  annotate("text", label = "2012", size = 2.5, x = 7, y = 2.47) +
  annotate("text", label = "2013", size = 2.5, x = 9, y = 2.435) +
  annotate("text", label = "2014", size = 2.5, x = 11, y = 2.389) +
  annotate("text", label = "2015", size = 2.5, x = 13, y = 2.477) +
  annotate("text", label = "2016", size = 2.5, x = 15, y = 2.53) +
  annotate("text", label = "2017", size = 2.5, x = 17, y = 2.49) +
  annotate("text", label = 'atop(bold("A"))', parse= TRUE, size = 7, x = 1, y = 3.38) +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
tree

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/trees/error_bar_line_trees_total.png", w = 20, h = 10, units = "cm", dpi = 300)


## por area


# CBO

arvores.cbo <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CBO", `Life Form` == "tree" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arvores.cbo


tree.cbo <- ggplot(arvores.cbo, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Trees abundances (CBO)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
tree.cbo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/trees/error_bar_line_tree_cbo.png", w = 20, h = 10, units = "cm", dpi = 300)


# VGM

arvores.vgm <- bt.dt.slc.gtr %>% 
  filter(`Site` == "VGM", `Life Form` == "tree" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arvores.vgm


tree.vgm <- ggplot(arvores.vgm, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Trees abundances (VGM)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
tree.vgm

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/trees/error_bar_line_tree_vgm.png", w = 20, h = 10, units = "cm", dpi = 300)


# ITA


arvores.ita <- bt.dt.slc.gtr %>% 
  filter(`Site` == "ITA", `Life Form` == "tree" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arvores.ita


tree.ita <- ggplot(arvores.ita, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Trees abundances (ITA)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
tree.ita

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/trees/error_bar_line_tree_ita.png", w = 20, h = 10, units = "cm", dpi = 300)



# CAR

arvores.car <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CAR", `Life Form` == "tree" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arvores.car


tree.car <- ggplot(arvores.car, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Trees abundances (CAR)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
tree.car

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/trees/error_bar_line_tree_car.png", w = 20, h = 10, units = "cm", dpi = 300)





### Ervas

ervas <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "herb" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
ervas


herb <- ggplot(ervas, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  theme(axis.title = element_text(size = 12, face = "bold"), legend.position = "none") +
  annotate("text", label = 'atop(bold("E"))', parse= TRUE, size = 7, x = 1, y = 2.98) +
  expand_limits(y=2) +
  labs(x = "Sampled period (months)", y = "Total herbs abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
herb

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/herbs/error_bar_line_herbs_total.png", w = 20, h = 10, units = "cm", dpi = 300)



## por area


# CBO

ervas.cbo <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CBO", `Life Form` == "herb" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
ervas.cbo


herb.cbo <- ggplot(ervas.cbo, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Herbs abundances (CBO)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
herb.cbo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/herbs/error_bar_line_herb_cbo.png", w = 20, h = 10, units = "cm", dpi = 300)


# VGM

ervas.vgm <- bt.dt.slc.gtr %>% 
  filter(`Site` == "VGM", `Life Form` == "herb" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
ervas.vgm


herb.vgm <- ggplot(ervas.vgm, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Herbs abundances (VGM)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
herb.vgm

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/herbs/error_bar_line_herb_vgm.png", w = 20, h = 10, units = "cm", dpi = 300)


# ITA


ervas.ita <- bt.dt.slc.gtr %>% 
  filter(`Site` == "ITA", `Life Form` == "herb" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
ervas.ita


herb.ita <- ggplot(ervas.ita, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Herbs abundances (ITA)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
herb.ita

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/herbs/error_bar_line_herb_ita.png", w = 20, h = 10, units = "cm", dpi = 300)



# CAR

ervas.car <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CAR", `Life Form` == "herb" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
ervas.car


herb.car <- ggplot(ervas.car, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Herbs abundances (CAR)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
herb.car

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/herbs/error_bar_line_herb_car.png", w = 20, h = 10, units = "cm", dpi = 300)





### lianas

lianas <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "liana" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
lianas


liana <- ggplot(lianas, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  theme(axis.title = element_text(size = 12, face = "bold"), legend.position = "none") +
  annotate("text", label = 'atop(bold("C"))', parse= TRUE, size = 7, x = 1, y =2.1) +
  expand_limits(y=2.1) +
  labs(x = "Sampled period (months)", y = "Total lianas abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
liana

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/lianas/error_bar_line_lianas_total.png", w = 20, h = 10, units = "cm", dpi = 300)





## por area


# CBO

lianas.cbo <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CBO", `Life Form` == "liana" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
lianas.cbo


liana.cbo <- ggplot(lianas.cbo, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  theme(axis.title = element_text(size = 12, face = "bold"), legend.position = "none") +
  annotate("text", label = 'atop(bold("D"))', parse= TRUE, size = 7, x = 1, y =2.1) +
  labs(x = "Sampled period (months)", y = "Lianas abundances (CBO)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
liana.cbo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/lianas/error_bar_line_liana_cbo.png", w = 20, h = 10, units = "cm", dpi = 300)


# VGM

lianas.vgm <- bt.dt.slc.gtr %>% 
  filter(`Site` == "VGM", `Life Form` == "liana" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
lianas.vgm


liana.vgm <- ggplot(lianas.vgm, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Lianas abundances (VGM)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
liana.vgm

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/lianas/error_bar_line_liana_vgm.png", w = 20, h = 10, units = "cm", dpi = 300)


# ITA


lianas.ita <- bt.dt.slc.gtr %>% 
  filter(`Site` == "ITA", `Life Form` == "liana" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
lianas.ita


liana.ita <- ggplot(lianas.ita, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Lianas abundances (ITA)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
liana.ita

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/lianas/error_bar_line_liana_ita.png", w = 20, h = 10, units = "cm", dpi = 300)



# CAR

lianas.car <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CAR", `Life Form` == "liana" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
lianas.car


liana.car <- ggplot(lianas.car, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Lianas abundances (CAR)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
liana.car

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/lianas/error_bar_line_liana_car.png", w = 20, h = 10, units = "cm", dpi = 300)






### bamboo

bamboos <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "bamboo" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
bamboos


bamboo <- ggplot(bamboos, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  theme(axis.title = element_text(size = 12, face = "bold"), legend.position = "none") +
  annotate("text", label = 'atop(bold("F"))', parse= TRUE, size = 7, x = 17, y =2.55) +
  expand_limits(y=2.5) +
  labs(x = "Sampled period (months)", y = "Total bamboos abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
bamboo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/bamboos/error_bar_line_bamboo_total.png", w = 20, h = 10, units = "cm", dpi = 300)


## por area


# CBO

bamboos.cbo <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CBO", `Life Form` == "bamboo" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
bamboos.cbo


bamboo.cbo <- ggplot(bamboos.cbo, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Bamboos abundances (CBO)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
bamboo.cbo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/bamboos/error_bar_line_bamboo_cbo.png", w = 20, h = 10, units = "cm", dpi = 300)


# VGM

bamboos.vgm <- bt.dt.slc.gtr %>% 
  filter(`Site` == "VGM", `Life Form` == "bamboo" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
bamboos.vgm


bamboo.vgm <- ggplot(bamboos.vgm, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Bamboos abundances (VGM)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
bamboo.vgm

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/bamboos/error_bar_line_bamboo_vgm.png", w = 20, h = 10, units = "cm", dpi = 300)


# ITA


bamboos.ita <- bt.dt.slc.gtr %>% 
  filter(`Site` == "ITA", `Life Form` == "bamboo" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
bamboos.ita


bamboo.ita <- ggplot(bamboos.ita, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Bamboos abundances (ITA)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
bamboo.ita

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/bamboos/error_bar_line_bamboo_ita.png", w = 20, h = 10, units = "cm", dpi = 300)



# CAR

bamboos.car <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CAR", `Life Form` == "bamboo" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
bamboos.car


bamboo.car <- ggplot(bamboos.car, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Bamboos abundances (CAR)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
bamboo.car

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/bamboos/error_bar_line_bamboo_car.png", w = 20, h = 10, units = "cm", dpi = 300)




### arbustos

arbustos <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "shrub" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arbustos


shrub <- ggplot(arbustos, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Total shrubs abundances (log)") +
  theme(axis.title = element_text(size = 12, face = "bold"), legend.position = "none") +
  annotate("text", label = 'atop(bold("D"))', parse= TRUE, size = 7, x = 17, y =2.32) +
  expand_limits(y=2.4) +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
shrub

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/shrubs/error_bar_line_shrubs_total.png", w = 20, h = 10, units = "cm", dpi = 300)




## por area


# CBO

arbustos.cbo <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CBO", `Life Form` == "shrub" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arbustos.cbo


shrub.cbo <- ggplot(arbustos.cbo, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Shrubs abundances (CBO)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
shrub.cbo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/shrubs/error_bar_line_shrub_cbo.png", w = 20, h = 10, units = "cm", dpi = 300)


# VGM

arbustos.vgm <- bt.dt.slc.gtr %>% 
  filter(`Site` == "VGM", `Life Form` == "shrub" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arbustos.vgm


shrub.vgm <- ggplot(arbustos.vgm, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Shrubs abundances (VGM)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
shrub.vgm

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/shrubs/error_bar_line_shrub_vgm.png", w = 20, h = 10, units = "cm", dpi = 300)


# ITA


arbustos.ita <- bt.dt.slc.gtr %>% 
  filter(`Site` == "ITA", `Life Form` == "shrub" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arbustos.ita


shrub.ita <- ggplot(arbustos.ita, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Shrubs abundances (ITA)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
shrub.ita

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/shrubs/error_bar_line_shrub_ita.png", w = 20, h = 10, units = "cm", dpi = 300)



# CAR

arbustos.car <- bt.dt.slc.gtr %>% 
  filter(`Site` == "CAR", `Life Form` == "shrub" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arbustos.car


shrub.car <- ggplot(arbustos.car, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar",
               width = 0.2,
               aes(group = Treatment),
               color = "black",
               fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               aes(shape = Treatment),
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               geom = "line",
               aes(group = Treatment),
               size = 1) +
  labs(x = "Sampled period (months)", y = "Shrubs abundances (CAR)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
shrub.car

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/shrubs/error_bar_line_shrub_car.png", w = 20, h = 10, units = "cm", dpi = 300)







































































### grafico de linhas com erro padrão - Modo 2

#library(plyr)

palm.summ <- ddply(palmeiras, c("Treatment", "time"), summarise,
               N    = length(abundancia),
               mean = mean(abundancia),
               sd   = sd(abundancia),
               se   = sd / sqrt(N)
)
palm.summ 



palm.2 <- ggplot(data = palm.summ , aes(x = time, y = mean, group = Treatment) ) + # lesion becomes a classifying factor
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.3) + # add error bars (do so before geom_point so the points are on top of the error bars)
  geom_line() + # join points with lines (specify this before geom_point, or the lines will be drawn over the shapes)
  geom_point(aes(shape=Treatment, fill=Treatment), size=5) + # add a scatterplot; constant size, shape/fill depends on lesion
  #scale_x_continuous("Session", breaks=1:12) + # have tick marks for each session
  #scale_y_continuous("Difference score", limits = c(-4, 8), breaks=seq(-4, 8, by = 2)) + # rescale Y axis slightly
  scale_shape_manual(values=c(24,21)) + # explicitly have sham=fillable triangle, ACCX=fillable circle
  scale_fill_manual(values=c("white","black")) + # explicitly have sham=white, ACCX=black
  #stat_abline(intercept=0, slope=0, linetype="dotted") + # add a reference line
  #annotate("text", x=11, y=-0.25, label="chance") + # and a manual label or annotation
  theme_bw()

palm.2
