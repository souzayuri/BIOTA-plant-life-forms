
# Informations ------------------------------------------------------------

### title: abundance of life forms ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 27/04/2020
### Description: This script creates a data table containing the density or abundance of each life form by subplots and plots. It also create an errorbar graphic to density of each life form by time until month 108.   



# Load packages and set directory -------------------------------------

rm(list = ls())

if(!require("tidyverse"))install.packages("tidyverse", dependencies = TRUE)
if(!require("textclean"))install.packages("textclean", dependencies = TRUE)
if(!require("ggpubr"))install.packages("ggpubr", dependencies = TRUE)

path <- "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao"
setwd(path)




# load data table ----------------------------------------------------------

data_biota <- read_csv("life_form_yuri_2020v1_para_abundancia.csv")
glimpse(data_biota)

data.biota.abun <- data_biota %>%
  dplyr::select(-c(24:28)) %>%   # remove unused columns
  gather(key = "Month", value = "value", 5:23) %>% 
  rename(life_form = `Life Form`) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  mutate(time = Month %>% stringr::str_replace("T", "")) %>% 
  textclean::drop_row("life_form", c("indeterminate", "fern")) %>%
  dplyr::group_by(Site, Plot, Treatment, Month, time, life_form) %>% 
  summarise(abundances = sum(value)) %>% 
  ungroup(Site, Plot, Treatment, Month, time, life_form)

data.biota.abun




# A-Trees -------------------------------------------------------------------


arvores <- data.biota.abun %>% 
  filter(life_form == "tree")
arvores


tree <- ggplot(arvores, aes(time, abundances, color = Treatment)) +
  scale_color_manual(values = c("tomato4", "seagreen"), name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
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
  labs(x = "Sampled period (months)", y = "Trees abundance") +
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
  annotate("text", label = 'atop(bold("A"))', parse= TRUE, size = 7, x = 1, y = 20.7) +
  expand_limits(y=2.5) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96", "102", "108"))
  
tree

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/abundance/ribbon_line_trees_abundance_month_27-04-20.png", w = 20, h = 10, units = "cm", dpi = 300)





# B-Palms -------------------------------------------------------------------


palmeiras <- data.biota.abun %>% 
  filter(`life_form` == "palm")
palmeiras

palm <- ggplot(palmeiras, aes(time, abundances, color = Treatment)) +
  scale_color_manual(values = c("tomato4", "seagreen"), name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
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
  labs(x = "", y = "Palms abundance") +
  annotate("text", label = 'atop(bold("B"))', parse= TRUE, size = 7, x = 18.6, y = 12.5) +
  #    annotate("text", label = "2009", size = 3, x = 1, y = 1.866) +
  #   annotate("text", label = "2010", size = 3, x = 3, y = 1.71) +
  #  annotate("text", label = "2011", size = 3, x = 5, y = 1.789) +
  # annotate("text", label = "2012", size = 3, x = 7, y = 1.97) +
  #     annotate("text", label = "2013", size = 3, x = 9, y = 1.68) +
  #    annotate("text", label = "2014", size = 3, x = 11, y = 1.71) +
  #   annotate("text", label = "2015", size = 3, x = 13, y = 1.81) +
  #  annotate("text", label = "2016", size = 3, x = 15, y = 1.71) +
  # annotate("text", label = "2017", size = 3, x = 17, y = 1.735) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96", "102", "108")) +
  expand_limits(y=1.4)
palm

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/abundance/ribbon_line_palm_abundance_month_27-04-20.png", w = 20, h = 10, units = "cm", dpi = 300)






# C-Lianas -------------------------------------------------------------------


lianas <- data.biota.abun %>% 
  filter(`life_form` == "liana")
lianas

liana <- ggplot(lianas, aes(time, abundances, color = Treatment)) +
  scale_color_manual(values = c("tomato4", "seagreen"), name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
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
  labs(x = "", y = "Lianas abundance") +
  annotate("text", label = 'atop(bold("C"))', parse= TRUE, size = 7, x = 1, y = 10.43) +
  #    annotate("text", label = "2009", size = 3, x = 1, y = 1.866) +
  #   annotate("text", label = "2010", size = 3, x = 3, y = 1.71) +
  #  annotate("text", label = "2011", size = 3, x = 5, y = 1.789) +
  # annotate("text", label = "2012", size = 3, x = 7, y = 1.97) +
  #     annotate("text", label = "2013", size = 3, x = 9, y = 1.68) +
  #    annotate("text", label = "2014", size = 3, x = 11, y = 1.71) +
  #   annotate("text", label = "2015", size = 3, x = 13, y = 1.81) +
  #  annotate("text", label = "2016", size = 3, x = 15, y = 1.71) +
  # annotate("text", label = "2017", size = 3, x = 17, y = 1.735) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96","102","108")) +
  expand_limits(y=1.2)
liana

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/abundance/ribbon_line_lianas_abundance_month_27-04-20.png", w = 20, h = 10, units = "cm", dpi = 300)






# D-Shrubs -------------------------------------------------------------------

arbusto <- data.biota.abun %>% 
  filter(`life_form` == "shrub")
arbusto

shrub <- ggplot(arbusto, aes(time, abundances, color = Treatment)) +
  scale_color_manual(values = c("tomato4", "seagreen"), name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
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
  labs(x = "", y = "Shrubs abundance") +
  annotate("text", label = 'atop(bold("D"))', parse= TRUE, size = 7, x = 18.6, y = 7.5) +
  #    annotate("text", label = "2009", size = 3, x = 1, y = 1.866) +
  #   annotate("text", label = "2010", size = 3, x = 3, y = 1.71) +
  #  annotate("text", label = "2011", size = 3, x = 5, y = 1.789) +
  # annotate("text", label = "2012", size = 3, x = 7, y = 1.97) +
  #     annotate("text", label = "2013", size = 3, x = 9, y = 1.68) +
  #    annotate("text", label = "2014", size = 3, x = 11, y = 1.71) +
  #   annotate("text", label = "2015", size = 3, x = 13, y = 1.81) +
  #  annotate("text", label = "2016", size = 3, x = 15, y = 1.71) +
  # annotate("text", label = "2017", size = 3, x = 17, y = 1.735) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96","102","108")) +
  expand_limits(y=1)
shrub

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/abundance/ribbon_line_shrubs_abundance_month_27-04-20.png", w = 20, h = 10, units = "cm", dpi = 300)





# E-Herbs -------------------------------------------------------------------

ervas <- data.biota.abun %>% 
  filter(`life_form` == "herb")
ervas

herb <- ggplot(ervas, aes(time, abundances, color = Treatment)) +
  scale_color_manual(values = c("tomato4", "seagreen"), name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
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
  labs(x = "", y = "Herbs abundance") +
  annotate("text", label = 'atop(bold("E"))', parse= TRUE, size = 7, x = 1, y = 14.02) +
  #    annotate("text", label = "2009", size = 3, x = 1, y = 1.866) +
  #   annotate("text", label = "2010", size = 3, x = 3, y = 1.71) +
  #  annotate("text", label = "2011", size = 3, x = 5, y = 1.789) +
  # annotate("text", label = "2012", size = 3, x = 7, y = 1.97) +
  #     annotate("text", label = "2013", size = 3, x = 9, y = 1.68) +
  #    annotate("text", label = "2014", size = 3, x = 11, y = 1.71) +
  #   annotate("text", label = "2015", size = 3, x = 13, y = 1.81) +
  #  annotate("text", label = "2016", size = 3, x = 15, y = 1.71) +
  # annotate("text", label = "2017", size = 3, x = 17, y = 1.735) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96","102","108")) +
  expand_limits(y=1.60)
herb

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/abundance/ribbon_line_herb_abundance_month_27-04-20.png", w = 20, h = 10, units = "cm", dpi = 300)






# F-bamboos -------------------------------------------------------------------

bamboos <- data.biota.abun %>% 
  filter(`life_form` == "bamboo")
bamboos

bamboo <- ggplot(bamboos, aes(time, abundances, color = Treatment)) +
  scale_color_manual(values = c("tomato4", "seagreen"), name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
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
  labs(x = "", y = "Bamboos abundance") +
  annotate("text", label = 'atop(bold("F"))', parse= TRUE, size = 7, x = 18.6, y = 7.6) +
  #    annotate("text", label = "2009", size = 3, x = 1, y = 1.866) +
  #   annotate("text", label = "2010", size = 3, x = 3, y = 1.71) +
  #  annotate("text", label = "2011", size = 3, x = 5, y = 1.789) +
  # annotate("text", label = "2012", size = 3, x = 7, y = 1.97) +
  #     annotate("text", label = "2013", size = 3, x = 9, y = 1.68) +
  #    annotate("text", label = "2014", size = 3, x = 11, y = 1.71) +
  #   annotate("text", label = "2015", size = 3, x = 13, y = 1.81) +
  #  annotate("text", label = "2016", size = 3, x = 15, y = 1.71) +
  # annotate("text", label = "2017", size = 3, x = 17, y = 1.735) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96","102","108")) +
  expand_limits(y=0.8)
bamboo

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/abundance/ribbon_line_bamboos_abundance_month_27-04-20.png", w = 20, h = 10, units = "cm", dpi = 300)


# ggarrange ---------------------------------------------------------------


ggarrange(tree, palm, liana, shrub, herb, bamboo, ncol = 2, nrow = 3)
ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/abundance/all_life_forms_abundance_month_27-04-20.png", w = 40, h = 35, units = "cm", dpi = 300)

