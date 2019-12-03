### title: Line graph with error bars ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 10/17/2019
### Description: Abundancia de grupos em cada parcela e em cada amostragem por tratamento em um grafico linear com desvio padrao

rm(list = ls()) 

library(tidyverse)
library(boot)

bt.dt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
bt.dt 


bt.dt.slc.gtr <- bt.dt %>% 
  select(-c(1,5,23:28)) %>% 
  gather(key = "time", value = "value", 4:20) %>% 
  rename(month = time) %>%
  mutate(time = month %>% stringr::str_replace("p", ""))
bt.dt.slc.gtr




# grafico de erro padrao --------------------------------------------------



### Arvores

arvores <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "tree" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, month, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arvores


tree <- ggplot(arvores, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  #stat_summary(fun.data = mean_cl_boot,
  #             geom = "errorbar",
  #             width = 0.2,
  #             aes(group = Treatment),
  #             color = "black",
  #             fun.args = list(conf.int = .95, B = 2000)) +
  stat_summary(fun.data = mean_cl_boot,
               geom = "ribbon",
               aes(group = Treatment),
               color = "0.12",
               alpha = 0.12,
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
  labs(x = "Sampled period (months)", y = "Trees abundances (log)") +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = c(0.15, .85),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  #annotate("text", label = "2009", size = 2.5, x = 1, y = 2.29) +
  #annotate("text", label = "2010", size = 2.5, x = 3, y = 2.399) +
  #annotate("text", label = "2011", size = 2.5, x = 5, y = 2.4) +
  #annotate("text", label = "2012", size = 2.5, x = 7, y = 2.47) +
  #annotate("text", label = "2013", size = 2.5, x = 9, y = 2.435) +
  #annotate("text", label = "2014", size = 2.5, x = 11, y = 2.389) +
  #annotate("text", label = "2015", size = 2.5, x = 13, y = 2.477) +
  #annotate("text", label = "2016", size = 2.5, x = 15, y = 2.53) +
  #annotate("text", label = "2017", size = 2.5, x = 17, y = 2.49) +
  annotate("text", label = 'atop(bold("G"))', parse= TRUE, size = 7, x = 1, y = 3.38) 
  #scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
tree

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/total_groups/ribbon_line_trees_total_month.png", w = 20, h = 10, units = "cm", dpi = 300)




### Palmeiras

palmeiras <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "palm" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, month, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
palmeiras


palm <- ggplot(palmeiras, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "ribbon",
               aes(group = Treatment),
               color = "0.12",
               alpha = 0.12,
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
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "none") +
  labs(x = "", y = "Palms abundances  (log)") +
  annotate("text", label = 'atop(bold("B"))', parse= TRUE, size = 7, x = 17.3, y = 2.90) +
  #    annotate("text", label = "2009", size = 3, x = 1, y = 1.866) +
  #   annotate("text", label = "2010", size = 3, x = 3, y = 1.71) +
  #  annotate("text", label = "2011", size = 3, x = 5, y = 1.789) +
  # annotate("text", label = "2012", size = 3, x = 7, y = 1.97) +
  #     annotate("text", label = "2013", size = 3, x = 9, y = 1.68) +
  #    annotate("text", label = "2014", size = 3, x = 11, y = 1.71) +
  #   annotate("text", label = "2015", size = 3, x = 13, y = 1.81) +
  #  annotate("text", label = "2016", size = 3, x = 15, y = 1.71) +
  # annotate("text", label = "2017", size = 3, x = 17, y = 1.735) +
  #scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96")) +
  expand_limits(y=3)
palm
ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/total_groups/ribbon_line_palm_total_month.png", w = 20, h = 10, units = "cm", dpi = 300)





### Ervas

ervas <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "herb" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, month, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
ervas


herb <- ggplot(ervas, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "ribbon",
               aes(group = Treatment),
               color = "0.12",
               alpha = 0.12,
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
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), legend.position = "none") +
  annotate("text", label = 'atop(bold("E"))', parse= TRUE, size = 7, x = 1, y = 2.98) +
  expand_limits(y=2) +
  labs(x = "", y = "Herbs abundances (log)") 
  #scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
herb

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/total_groups/ribbon_line_herbs_total_month.png", w = 20, h = 10, units = "cm", dpi = 300)




### lianas

lianas <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "liana" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, month, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
lianas


liana <- ggplot(lianas, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "ribbon",
               aes(group = Treatment),
               color = "0.12",
               alpha = 0.12,
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
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), legend.position = "none") +
  annotate("text", label = 'atop(bold("C"))', parse= TRUE, size = 7, x = 1, y =2.1) +
  expand_limits(y=2.1) +
  labs(x = "", y = "Lianas abundances (log)") 
  #scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
liana

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/total_groups/ribbon_line_lianas_total_month.png", w = 20, h = 10, units = "cm", dpi = 300)




### bamboo

bamboos <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "bamboo" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, month, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
bamboos


bamboo <- ggplot(bamboos, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "ribbon",
               aes(group = Treatment),
               color = "0.12",
               alpha = 0.12,
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
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), legend.position = "none") +
  annotate("text", label = 'atop(bold("F"))', parse= TRUE, size = 7, x = 17.3, y =2.55) +
  expand_limits(y=2.5) +
  labs(x = "", y = "Bamboos abundances (log)") 
  #scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
bamboo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/total_groups/ribbon_line_bamboo_total_month.png", w = 20, h = 10, units = "cm", dpi = 300)




### arbustos

arbustos <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "shrub" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, month, `Life Form`) %>% 
  summarise(abundancia = log(sum(value+1))) %>% 
  rename(life_form = `Life Form`)
arbustos


shrub <- ggplot(arbustos, aes(time, abundancia, color = Treatment)) +
  scale_color_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  stat_summary(fun.data = mean_cl_boot,
               geom = "ribbon",
               aes(group = Treatment),
               color = "0.12",
               alpha = 0.12,
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
  labs(x = "", y = "Shrubs abundances (log)") +
  theme(axis.title = element_text(size = 20), axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), legend.position = "none") +
  annotate("text", label = 'atop(bold("D"))', parse= TRUE, size = 7, x = 17.3, y =2.32) +
  expand_limits(y=2.4) 
  #scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))
shrub

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/error_bar_line/total_groups/ribbon_line_shrubs_total_month.png", w = 20, h = 10, units = "cm", dpi = 300)
