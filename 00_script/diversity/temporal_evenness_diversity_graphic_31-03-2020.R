# Informations ------------------------------------------------------------

### title: Temporal evenness diversity graphic  ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 30/03/2020
### Description: this script plot 2 graphics with time variation

rm(list = ls())


# packages -----------------------------------------------------------------


library(tidyverse)
library(textclean)
library(vegan)
library(ggpubr)
library(gridExtra)
library(gtable)
library(grid)
library(codyn)

citation("vegan")

# Manipulating datatable ------------------------------------------------

data_biota_diver <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/diversity/total.lf.diversity_31-03-20.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota_diver)

data.biota.div.mean <- data_biota_diver %>% 
  select(-c(1:3)) %>% 
  dplyr::group_by(time, treatment, life_form) %>% 
  summarise(mean_simpson = mean(SimpsonEvenness),
            mean_pilou = mean(pilou_evenness),
            total_abundance = sum(abundances),
            total_richness = sum(richness)) %>% 
  dplyr::ungroup(time, treatment, life_form) %>% 
  mutate(life_form = recode(life_form, 
                            "bamboos" = "Bamboos","herbs" = "Herbs",
                            "shrubs" = "Shrubs","lianas" = "Lianas",
                            "trees" = "Trees", "palms" = "Palms")) %>% 
  arrange(match(life_form, c("Trees", "Palms","Lianas",
                             "Shrubs","Herbs","Bamboos"))) %>% 
  mutate(time = recode(time, 
                        "0" = 2010, "6" = 2011,
                        "12" = 2011, "18" = 2012, 
                        "24" = 2012, "30" = 2013,
                        "36" = 2013, "42" = 2014,
                        "48" = 2014, "54" = 2015,
                        "60" = 2015, "66" = 2016,
                        "72" = 2016, "78" = 2017,
                        "84" = 2017, "90" = 2018,
                        "96" = 2018)) %>% 
  mutate(life_form = factor(life_form, level = c("Trees", "Palms","Lianas",
                               "Shrubs","Herbs","Bamboos")))

glimpse(data.biota.div.mean)
data.biota.div.mean


# graphic simpson evenness - open ----------------------------------------------------------


lf_div_open <- ggplot(subset(data.biota.div.mean, treatment %in% ("open")), 
                      aes(x= total_abundance, y= mean_simpson, size = time, 
                          fill = life_form, colour = life_form)) + 
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Simpson's evenness", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  guides(colour = guide_legend(override.aes = list(size=3, stroke=3),order = 2),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1)) +
  theme_bw() + theme(text = element_text(size = 14),
                     axis.text = element_text(size = 13),
                     legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))

lf_div_open

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpsons_evenness_diversity_lf_open.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)



# grafico arrange legend - open ----------------------------------------------------------


lf.geomlp.div.open <- ggplot(subset(data.biota.div.mean, treatment %in% ("open")), 
                         aes(x= total_abundance, y= mean_simpson, size = time,
                             fill = life_form, colour = life_form)) + 
  geom_point(aes(fill = life_form), shape = 21, size = 3) +
  geom_line(aes(colour = life_form), linetype = "solid", size = 1) + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) + 
  guides(colour = guide_legend(override.aes = list(size=3, stroke=2), ncol = 2)) +
  labs(colour="Life-forms") + 
  theme(legend.title = element_text(face = "bold", size = 25),
        legend.text = element_text(size = 25)) +
  theme_bw() +  theme(legend.direction = "horizontal",
                      text = element_text(size = 14),
                      axis.text = element_text(size = 13),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 10))
 
lf.geomlp.div.open

lf.legend.div.open <- gtable_filter(ggplot_gtable(ggplot_build(lf.geomlp.div.open)), "guide-box") 
lf.legend.div.open



mon.geomalpha.div.open <- ggplot(subset(data.biota.div.mean, treatment %in% ("open")), 
                             aes(x= total_abundance, y= mean_simpson, size = time)) + 
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Fisher's"~alpha, x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  guides(colour = guide_legend(override.aes = list(size=3, stroke=3),order = 2),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1)) +
  theme_bw() + theme(legend.direction = "horizontal",
                        text = element_text(size = 14),
                        axis.text = element_text(size = 13),
                        legend.title = element_text(size = 10),
                        legend.text = element_text(size = 10))

mon.geomalpha.div.open

mon.legend.div.open <- gtable_filter(ggplot_gtable(ggplot_build(mon.geomalpha.div.open)), "guide-box") 
mon.legend.div.open


unlegend.div.open <- ggplot(subset(data.biota.div.mean, treatment %in% ("open")), 
                        aes(x= total_abundance, y= mean_simpson, size = time,
                            fill = life_form, colour = life_form)) + 
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12), guide = FALSE) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15), guide = FALSE) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown"), guide = FALSE) +
  labs(y = "Simpson's evenness", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw() + theme(legend.direction = "horizontal",
                     text = element_text(size = 14),
                     axis.text = element_text(size = 13),
                     legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))

unlegend.div.open


join.lf.unl.div.o <- arrangeGrob(lf.legend.div.open, unlegend.div.open, 
                             heights = unit.c(lf.legend.div.open$height, unit(1, "npc") - lf.legend.div.open$height), ncol = 1)

join.lf.mon.div.unl.o <- arrangeGrob(join.lf.unl.div.o, mon.legend.div.open,
                                 widths = unit.c(unit(1, "npc") - mon.legend.div.open$width, mon.legend.div.open$width), nrow = 1)

grid.newpage()
grid.draw(join.lf.mon.div.unl.o)


plot.lf.div.open <- unlegend.div.open + 
  annotation_custom(grob = lf.legend.div.open, xmin = 300, xmax = 350, ymin = 0.6, ymax = 0.7)
plot.lf.div.open

plot.lf.mon.div.open <- plot.lf.div.open + 
  annotation_custom(grob = mon.legend.div.open, xmin = 130, xmax = 230, ymin = 0.50, ymax = 0.75)
plot.lf.mon.div.open

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpson_evenness_diversity_lf_legend_arranged_open.jpeg", width = 15, height = 17, units = "cm", dpi = 300)




# grafico arrange unlegend - open --------------------------------------------------------

plot.div.unlegend.open<- ggplot(subset(data.biota.div.mean, treatment %in% ("open")), 
                                   aes(x= total_abundance, y= mean_simpson, size = time, 
                                       fill = life_form, colour = life_form)) +
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12), guide = FALSE) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15), guide = FALSE) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown"), guide = FALSE) +
  labs(y = "Simpson's evenness", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw() + theme(text = element_text(size = 14),
                     axis.text = element_text(size = 13),
                     legend.title = element_text(size = 20),
                     legend.text = element_text(size = 20))

plot.div.unlegend.open 

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpson_evenness_diversity_lf_unlegend_closed.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)


# grafico arrange legend - closed ----------------------------------------------------------


lf.geomlp.div.closed <- ggplot(subset(data.biota.div.mean, treatment %in% ("closed")), 
                             aes(x= total_abundance, y= mean_simpson, size = time,
                                 fill = life_form, colour = life_form)) + 
  geom_point(aes(fill = life_form), shape = 21, size = 3) +
  geom_line(aes(colour = life_form), linetype = "solid", size = 1) + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) + 
  guides(colour = guide_legend(override.aes = list(size=3, stroke=2), ncol = 2)) +
  labs(colour="Life-forms") + 
  theme(legend.title = element_text(face = "bold", size = 25),
        legend.text = element_text(size = 25)) +
  theme_bw() + theme(legend.direction = "horizontal",
                     text = element_text(size = 14),
                     axis.text = element_text(size = 13),
                     legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))

lf.geomlp.div.closed

lf.legend.div.closed <- gtable_filter(ggplot_gtable(ggplot_build(lf.geomlp.div.closed)), "guide-box") 
lf.legend.div.closed



mon.geomalpha.div.closed <- ggplot(subset(data.biota.div.mean, treatment %in% ("closed")), 
                                 aes(x= total_abundance, y= mean_simpson, size = time)) + 
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Simpson evenness", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  guides(colour = guide_legend(override.aes = list(size=3, stroke=3),order = 2),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1)) +
  theme_bw() + theme(legend.direction = "horizontal",
                      text = element_text(size = 14),
                      axis.text = element_text(size = 13),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 10))

mon.geomalpha.div.closed

mon.legend.div.closed <- gtable_filter(ggplot_gtable(ggplot_build(mon.geomalpha.div.closed)), "guide-box") 
mon.legend.div.closed


unlegend.div.closed <- ggplot(subset(data.biota.div.mean, treatment %in% ("closed")), 
                            aes(x= total_abundance, y= mean_simpson, size = time,
                                fill = life_form, colour = life_form)) + 
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12), guide = FALSE) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15), guide = FALSE) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown"), guide = FALSE) +
  labs(y = "Simpson's evenness", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw() + theme(legend.direction = "horizontal",
                      text = element_text(size = 14),
                      axis.text = element_text(size = 13),
                      legend.title = element_text(size = 10),
                      legend.text = element_text(size = 10))

unlegend.div.closed


join.lf.unl.div.c <- arrangeGrob(lf.legend.div.closed, unlegend.div.closed, 
                                 heights = unit.c(lf.legend.div.closed$height, unit(1, "npc") - lf.legend.div.closed$height), ncol = 1)

join.lf.mon.div.unl.c <- arrangeGrob(join.lf.unl.div.c, mon.legend.div.closed,
                                     widths = unit.c(unit(1, "npc") - mon.legend.div.closed$width, mon.legend.div.closed$width), nrow = 1)

grid.newpage()
grid.draw(join.lf.mon.div.unl.c)


plot.lf.div.closed <- unlegend.div.closed + 
  annotation_custom(grob = lf.legend.div.closed, xmin = 300, xmax = 350, ymin = 0.6, ymax = 0.7)
plot.lf.div.closed

plot.lf.mon.div.closed <- plot.lf.div.closed + 
  annotation_custom(grob = mon.legend.div.closed, xmin = 130, xmax = 230, ymin = 0.50, ymax = 0.75)
plot.lf.mon.div.closed

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpson_evenness_diversity_lf_legend_arranged_open.jpeg", width = 15, height = 17, units = "cm", dpi = 300)




# graphic simpson evenness - closed -------------------------------------------------------------------

lf_div_closed <- ggplot(subset(data.biota.div.mean, treatment %in% ("closed")), 
                      aes(x= total_abundance, y= mean_simpson, size = time, 
                          fill = life_form, colour = life_form)) +
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Simpson's evenness", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  guides(colour = guide_legend(override.aes = list(size=3, stroke=3),order = 2),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1)) +
  theme_bw() + theme(legend.direction = "horizontal",
                     text = element_text(size = 14),
                     axis.text = element_text(size = 13),
                     legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))

lf_div_closed

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpson_evenness_diversity_lf_closed.jpeg", width = 15, height = 17, units = "cm", dpi = 300)



# grafico arrange unlegend - closed --------------------------------------------------------

plot.div.unlegend.closed <- ggplot(subset(data.biota.div.mean, treatment %in% ("closed")), 
                        aes(x= total_abundance, y= mean_simpson, size = time, 
                            fill = life_form, colour = life_form)) +
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = time), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12), guide = FALSE) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15), guide = FALSE) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown"), guide = FALSE) +
  labs(y = "Simpson's evenness", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw() + theme(legend.direction = "horizontal",
                     text = element_text(size = 14),
                     axis.text = element_text(size = 13),
                     legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))

plot.div.unlegend.closed 

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpson_evenness_diversity_lf_unlegend_closed.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)


# join graphics --------------------------------------------------------


theme_set(theme_pubr())

ggarrange(
  lf_div_open, lf_div_closed + rremove("ylab"),
  ncol = 2, nrow = 1, labels  = "AUTO",
  common.legend = TRUE, legend = "right",
  hjust = c(-4, -2.0), font.label = list(size = 14, color = "black", face =
                                           "bold", family = NULL))


#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpson_evenness_diversity_lf_join.jpeg", width = 25, height = 17, units = "cm", dpi = 1000)


# join graphics with arrenged ---------------------------------------------

plot.lf.div.closed <- unlegend.div.closed + 
  annotation_custom(grob = lf.legend.div.closed, xmin = 240, xmax = 420, ymin = 0.62, ymax = 0.67)
plot.lf.div.closed

plot.lf.div.mon.closed <- plot.lf.div.closed + 
  annotation_custom(grob = mon.legend.div.closed, xmin = 260, xmax = 720, ymin = 0.54, ymax = 0.64)
plot.lf.div.mon.closed

theme_set(theme_pubr())

ggarrange(
  plot.div.unlegend.open, plot.lf.div.mon.closed + rremove("ylab"),
  ncol = 2, nrow = 1, labels  = "AUTO",
  common.legend = TRUE, legend = "right",
  hjust = c(-4, -2.0), font.label = list(size = 14, color = "black", face =
                                           "bold", family = NULL))

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/simpson_evenness_diversity_lf_join_arranged.jpeg", width = 35, height = 20, units = "cm", dpi = 300)
