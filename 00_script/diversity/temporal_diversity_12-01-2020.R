# Informations ------------------------------------------------------------

### title: Temporal diversity abundance ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 14/01/2020
### Description: this script calculates the plant life-form species diversity and plot it in a graphic with their abundance varying in time

rm(list = ls())


# packages -----------------------------------------------------------------


library(tidyverse)
library(textclean)
library(vegan)
library(ggpubr)
library(gridExtra)
library(gtable)
library(grid)

citation("vegan")

# Manipulating datatable ------------------------------------------------

data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv", locale = locale(encoding = "ASCII")) # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)

# Open treatment --------------------------------------------------------

# Trees species abundances - Open ---------------------------------------


data.biota.div.tre.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "tree") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.tre.o

#which(is.na(data.biota.div.tre.o))

data.biota.div.tree.o <- data.biota.div.tre.o %>% 
  pivot_wider(names_from = Species, 
              values_from = abundances)

data.biota.div.tree.o

data.biota.div.tree.o <- data.biota.div.tree.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.tree.o

alfa.tree.o <- fisher.alpha(data.biota.div.tree.o)
alfa.tree.o
summary(alfa.tree.o)

alfa.tree.o <- as.data.frame(alfa.tree.o)
alfa.tree.o

alfa.tree.o.rn <- rownames_to_column(alfa.tree.o, (var="Month"))
alfa.tree.o.rn$Month <- as.numeric(alfa.tree.o.rn$Month)+1
alfa.tree.o.rn$Month <- log(alfa.tree.o.rn$Month)
glimpse(alfa.tree.o.rn)
alfa.tree.o.lm <- lm(alfa.tree.o~Month, data = alfa.tree.o.rn)
summary(alfa.tree.o.lm)
#plot(alfa.tree.o.lm)
shapiro.test(alfa.tree.o.lm$residuals)

# Palms species abundances - Open ---------------------------------------


data.biota.div.pal.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "palm") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.pal.o

#which(is.na(data.biota.div))

data.biota.div.palm.o <- data.biota.div.pal.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.palm.o

data.biota.div.palm.o <- data.biota.div.palm.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.palm.o

alfa.palm.o <- fisher.alpha(data.biota.div.palm.o)
alfa.palm.o
summary(alfa.palm.o)


alfa.palm.o <- as.data.frame(alfa.palm.o)
alfa.palm.o

alfa.palm.o.rn <- rownames_to_column(alfa.palm.o, (var="Month"))
alfa.palm.o.rn$Month <- as.numeric(alfa.palm.o.rn$Month)+1
alfa.palm.o.rn$Month <- log(alfa.palm.o.rn$Month)
glimpse(alfa.palm.o.rn)
alfa.palm.o.lm <- lm(alfa.palm.o~Month, data = alfa.palm.o.rn)
summary(alfa.palm.o.lm)
#plot(alfa.palm.o.lm)
shapiro.test(alfa.palm.o.lm$residuals)


# Lianas species abundances - open ---------------------------------------


data.biota.div.lia.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "liana") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.lia.o

#which(is.na(data.biota.div))

data.biota.div.liana.o <- data.biota.div.lia.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.liana.o

data.biota.div.liana.o <- data.biota.div.liana.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.liana.o

alfa.liana.o <- fisher.alpha(data.biota.div.liana.o)
alfa.liana.o
summary(alfa.liana.o)

alfa.liana.o <- as.data.frame(alfa.liana.o)
alfa.liana.o

alfa.liana.o.rn <- rownames_to_column(alfa.liana.o, (var="Month"))
alfa.liana.o.rn$Month <- as.numeric(alfa.liana.o.rn$Month)+1
alfa.liana.o.rn$Month <- log(alfa.liana.o.rn$Month)
glimpse(alfa.liana.o.rn)
alfa.liana.o.lm <- lm(alfa.liana.o~Month, data = alfa.liana.o.rn)
summary(alfa.liana.o.lm)
#plot(alfa.liana.o.lm)
shapiro.test(alfa.liana.o.lm$residuals)



# Shrubs species abundances - open ---------------------------------------


data.biota.div.shr.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "shrub") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.shr.o

#which(is.na(data.biota.div))

data.biota.div.shrub.o <- data.biota.div.shr.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.shrub.o

data.biota.div.shrub.o <- data.biota.div.shrub.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.shrub.o


alfa.shrub.o <- fisher.alpha(data.biota.div.shrub.o)
alfa.shrub.o
summary(alfa.shrub.o)


alfa.shrub.o <- as.data.frame(alfa.shrub.o)
alfa.shrub.o


alfa.shrub.o.rn <- rownames_to_column(alfa.shrub.o, (var="Month"))
alfa.shrub.o.rn$Month <- as.numeric(alfa.shrub.o.rn$Month)+1
alfa.shrub.o.rn$Month <- log(alfa.shrub.o.rn$Month)
glimpse(alfa.shrub.o.rn)
alfa.shrub.o.lm <- lm(alfa.shrub.o~Month, data = alfa.shrub.o.rn)
summary(alfa.shrub.o.lm)
#plot(alfa.shrub.o.lm)
shapiro.test(alfa.shrub.o.lm$residuals)



# Herbs species abundances - open ---------------------------------------


data.biota.div.her.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "herb") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.her.o

#which(is.na(data.biota.div))

data.biota.div.herb.o <- data.biota.div.her.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.herb.o

data.biota.div.herb.o <- data.biota.div.herb.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.herb.o

alfa.herb.o <- fisher.alpha(data.biota.div.herb.o)
alfa.herb.o
summary(alfa.herb.o)


alfa.herb.o <- as.data.frame(alfa.herb.o)
alfa.herb.o

alfa.herb.o.rn <- rownames_to_column(alfa.herb.o, (var="Month"))
alfa.herb.o.rn$Month <- as.numeric(alfa.herb.o.rn$Month)+1
alfa.herb.o.rn$Month <- log(alfa.herb.o.rn$Month)
glimpse(alfa.herb.o.rn)
alfa.herb.o.lm <- lm(alfa.herb.o~Month, data = alfa.herb.o.rn)
summary(alfa.herb.o.lm)
#plot(alfa.herb.o.lm)
shapiro.test(alfa.herb.o.lm$residuals)


# Bamboos species abundances - open ---------------------------------------


data.biota.div.bam.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "bamboo") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.bam.o


data.biota.div.bamboo.o <- data.biota.div.bam.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.bamboo.o

data.biota.div.bamboo.o <- data.biota.div.bamboo.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.bamboo.o

alfa.bamboo.o <- fisher.alpha(data.biota.div.bamboo.o)
alfa.bamboo.o
summary(alfa.bamboo.o)

alfa.bamboo.o <- as.data.frame(alfa.bamboo.o)
alfa.bamboo.o

alfa.bamboo.o.rn <- rownames_to_column(alfa.bamboo.o, (var="Month"))
alfa.bamboo.o.rn$Month <- as.numeric(alfa.bamboo.o.rn$Month)+1
alfa.bamboo.o.rn$Month <- log(alfa.bamboo.o.rn$Month)
glimpse(alfa.bamboo.o.rn)
alfa.bamboo.o.lm <- lm(alfa.bamboo.o~Month, data = alfa.bamboo.o.rn)
summary(alfa.bamboo.o.lm)
#plot(alfa.bamboo.o.lm)
shapiro.test(alfa.bamboo.o.lm$residuals)

# creating alpha and abundance table - open --------------------------------------


life_form_df_o <- cbind(alfa.tree.o,alfa.palm.o,alfa.liana.o,
                        alfa.shrub.o,alfa.herb.o,alfa.bamboo.o)

life_form_df_o 

life_form_o <- rownames_to_column(life_form_df_o, (var="Month")) %>% 
  gather(key = "life_form", value = "value", 2:7) %>% 
  rename(fisher_alpha = value) %>% 
  mutate(life_form = recode(life_form, "alfa.tree.o" = "Trees", 
                            "alfa.palm.o" = "Palms",
                            "alfa.liana.o" = "Lianas",
                            "alfa.shrub.o" = "Shrubs",
                            "alfa.herb.o" = "Herbs",
                            "alfa.bamboo.o" = "Bamboos")) %>% 
  mutate(life_form = factor(life_form))

life_form_o

data.biota.oln.spp.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>% 
  textclean::drop_row("life_form", c("indeterminate")) %>% 
  dplyr::group_by(life_form, Month) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%
  dplyr::ungroup(life_form, Month) %>%
  mutate(life_form = recode(life_form, 
                            "bamboo" = "Bamboos","herb" = "Herbs",
                            "shrub" = "Shrubs","liana" = "Lianas",
                            "tree" = "Trees", "palm" = "Palms")) %>% 
  dplyr::select(Month,life_form,abundances) %>% 
  arrange(match(life_form, c("Trees", "Palms","Lianas",
                             "Shrubs","Herbs","Bamboos")))

data.biota.oln.spp.o




life_form_o$abundances <- data.biota.oln.spp.o$abundances
life_form_o


life.form.ab.alpha.o <- life_form_o %>% 
  mutate(Month = recode(Month, 
                        "0" = 2010, "6" = 2011,
                        "12" = 2011, "18" = 2012, 
                        "24" = 2012, "30" = 2013,
                        "36" = 2013, "42" = 2014,
                        "48" = 2014, "54" = 2015,
                        "60" = 2015, "66" = 2016,
                        "72" = 2016, "78" = 2017,
                        "84" = 2017, "90" = 2018,
                        "96" = 2018))

glimpse(life.form.ab.alpha.o)


life.form.ab.alpha.o$life_form <- factor(life.form.ab.alpha.o$life_form, levels = c("Trees", "Palms","Lianas",
                                                                                    "Shrubs","Herbs","Bamboos"))
life.form.ab.alpha.o


# grafico fishe's alpha bubble life form - open -------------------------------------------------------------------

lf_open <- ggplot(life.form.ab.alpha.o, aes(x= abundances, y= fisher_alpha, size = Month, 
                                 fill = life_form, colour = life_form)) + 
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = Month), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Fisher's alpha", x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  guides(colour = guide_legend(override.aes = list(size=3, stroke=3),order = 2),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1)) +
  theme_bw() 

lf_open

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_open.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)


# grafico arrange legend ----------------------------------------------------------


lf.geomlp.open <- ggplot(life.form.ab.alpha.o, aes(x= abundances, y= fisher_alpha, 
                                                   fill = life_form, colour = life_form)) + 
  geom_point(aes(fill = life_form), shape = 21, size = 3) +
  geom_line(aes(colour = life_form), linetype = "solid", size = 1) + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) + 
  guides(colour = guide_legend(override.aes = list(size=3, stroke=2))) +
  labs(colour="Life-forms") + 
  theme(legend.title = element_text(face = "bold", size = 25),
        legend.text = element_text(size = 25)) +
  theme_bw()
lf.geomlp.open

lf.legend.open <- gtable_filter(ggplot_gtable(ggplot_build(lf.geomlp.open)), "guide-box") 
lf.legend.open



mon.geomalpha.open <- ggplot(life.form.ab.alpha.o, aes(x= abundances, y= fisher_alpha, size = Month)) + 
  geom_point(aes(alpha = Month), pch = 16) + 
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
  theme_bw() 

mon.geomalpha.open

mon.legend.open <- gtable_filter(ggplot_gtable(ggplot_build(mon.geomalpha.open)), "guide-box") 
mon.legend.open


unlegend.open <- ggplot(life.form.ab.alpha.o, aes(x= abundances, y= fisher_alpha, size = Month, 
                                                  fill = life_form, colour = life_form)) +
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = Month), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12), guide = FALSE) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15), guide = FALSE) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown"), guide = FALSE) +
  labs(y = "Fisher's"~alpha, x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw()

unlegend.open


join.lf.unl.o <- arrangeGrob(lf.legend.open, unlegend.open, 
                             heights = unit.c(lf.legend.open$height, unit(1, "npc") - lf.legend.open$height), ncol = 1)

join.lf.mon.unl.o <- arrangeGrob(join.lf.unl.o, mon.legend.open,
                                 widths = unit.c(unit(1, "npc") - mon.legend.open$width, mon.legend.open$width), nrow = 1)

grid.newpage()
grid.draw(join.lf.mon.unl.o)


plot.lf.open <- unlegend.open + 
  annotation_custom(grob = lf.legend.open, xmin = 80, xmax = 200, ymin = 20, ymax = 36.1)
plot.lf.open

plot.lf.mon.open <- plot.lf.open + 
  annotation_custom(grob = mon.legend.open, xmin = 20, xmax = 100, ymin = 10, ymax = 35)
plot.lf.mon.open

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_legend_arranged_open.jpeg", width = 15, height = 17, units = "cm", dpi = 300)


# Closed treatment --------------------------------------------------------

# Trees species abundances - Closed ---------------------------------------


data.biota.div.tre.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "tree") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.tre.c

#which(is.na(data.biota.div.tre.c))

data.biota.div.tree.c <- data.biota.div.tre.c %>% 
  pivot_wider(names_from = Species, 
              values_from = abundances)

data.biota.div.tree.c

data.biota.div.tree.c <- data.biota.div.tree.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.tree.c

alfa.tree.c <- fisher.alpha(data.biota.div.tree.c)
alfa.tree.c
summary(alfa.tree.c)

alfa.tree.c <- as.data.frame(alfa.tree.c)
alfa.tree.c

alfa.tree.c.rn <- rownames_to_column(alfa.tree.c, (var="Month"))
alfa.tree.c.rn$Month <- as.numeric(alfa.tree.c.rn$Month)+1
alfa.tree.c.rn$Month <- log(alfa.tree.c.rn$Month)
glimpse(alfa.tree.c.rn)
alfa.tree.c.lm <- lm(alfa.tree.c~Month, data = alfa.tree.c.rn)
summary(alfa.tree.c.lm)
#plot(alfa.tree.c.lm)
shapiro.test(alfa.tree.c.lm$residuals)

# Palms species abundances - Closed ---------------------------------------


data.biota.div.pal.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "palm") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.pal.c

#which(is.na(data.biota.div))

data.biota.div.palm.c <- data.biota.div.pal.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.palm.c

data.biota.div.palm.c <- data.biota.div.palm.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.palm.c

alfa.palm.c <- fisher.alpha(data.biota.div.palm.c)
alfa.palm.c
summary(alfa.palm.c)


alfa.palm.c <- as.data.frame(alfa.palm.c)
alfa.palm.c

alfa.palm.c.rn <- rownames_to_column(alfa.palm.c, (var="Month"))
alfa.palm.c.rn$Month <- as.numeric(alfa.palm.c.rn$Month)+1
alfa.palm.c.rn$Month <- log(alfa.palm.c.rn$Month)
glimpse(alfa.palm.c.rn)
alfa.palm.c.lm <- lm(alfa.palm.c~Month, data = alfa.palm.c.rn)
summary(alfa.palm.c.lm)
#plot(alfa.palm.c.lm)
shapiro.test(alfa.palm.c.lm$residuals)


# Lianas species abundances - Closed ---------------------------------------


data.biota.div.lia.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "liana") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.lia.c

#which(is.na(data.biota.div))

data.biota.div.liana.c <- data.biota.div.lia.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.liana.c

data.biota.div.liana.c <- data.biota.div.liana.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.liana.c

alfa.liana.c <- fisher.alpha(data.biota.div.liana.c)
alfa.liana.c
summary(alfa.liana.c)

alfa.liana.c <- as.data.frame(alfa.liana.c)
alfa.liana.c

alfa.liana.c.rn <- rownames_to_column(alfa.liana.c, (var="Month"))
alfa.liana.c.rn$Month <- as.numeric(alfa.liana.c.rn$Month)+1
alfa.liana.c.rn$Month <- log(alfa.liana.c.rn$Month)
glimpse(alfa.liana.c.rn)
alfa.liana.c.lm <- lm(alfa.liana.c~Month, data = alfa.liana.c.rn)
summary(alfa.liana.c.lm)
#plot(alfa.liana.c.lm)
shapiro.test(alfa.liana.c.lm$residuals)



# Shrubs species abundances - Closed ---------------------------------------


data.biota.div.shr.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "shrub") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.shr.c

#which(is.na(data.biota.div))

data.biota.div.shrub.c <- data.biota.div.shr.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.shrub.c

data.biota.div.shrub.c <- data.biota.div.shrub.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.shrub.c


alfa.shrub.c <- fisher.alpha(data.biota.div.shrub.c)
alfa.shrub.c
summary(alfa.shrub.c)


alfa.shrub.c <- as.data.frame(alfa.shrub.c)
alfa.shrub.c


alfa.shrub.c.rn <- rownames_to_column(alfa.shrub.c, (var="Month"))
alfa.shrub.c.rn$Month <- as.numeric(alfa.shrub.c.rn$Month)+1
alfa.shrub.c.rn$Month <- log(alfa.shrub.c.rn$Month)
glimpse(alfa.shrub.c.rn)
alfa.shrub.c.lm <- lm(alfa.shrub.c~Month, data = alfa.shrub.c.rn)
summary(alfa.shrub.c.lm)
#plot(alfa.shrub.c.lm)
shapiro.test(alfa.shrub.c.lm$residuals)



# Herbs species abundances - Closed ---------------------------------------


data.biota.div.her.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "herb") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.her.c

#which(is.na(data.biota.div))

data.biota.div.herb.c <- data.biota.div.her.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.herb.c

data.biota.div.herb.c <- data.biota.div.herb.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.herb.c

alfa.herb.c <- fisher.alpha(data.biota.div.herb.c)
alfa.herb.c
summary(alfa.herb.c)


alfa.herb.c <- as.data.frame(alfa.herb.c)
alfa.herb.c

alfa.herb.c.rn <- rownames_to_column(alfa.herb.c, (var="Month"))
alfa.herb.c.rn$Month <- as.numeric(alfa.herb.c.rn$Month)+1
alfa.herb.c.rn$Month <- log(alfa.herb.c.rn$Month)
glimpse(alfa.herb.c.rn)
alfa.herb.c.lm <- lm(alfa.herb.c~Month, data = alfa.herb.c.rn)
summary(alfa.herb.c.lm)
#plot(alfa.herb.c.lm)
shapiro.test(alfa.herb.c.lm$residuals)


# Bamboos species abundances - Closed ---------------------------------------


data.biota.div.bam.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "bamboo") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.bam.c


data.biota.div.bamboo.c <- data.biota.div.bam.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.bamboo.c

data.biota.div.bamboo.c <- data.biota.div.bamboo.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.bamboo.c

alfa.bamboo.c <- fisher.alpha(data.biota.div.bamboo.c)
alfa.bamboo.c
summary(alfa.bamboo.c)

alfa.bamboo.c <- as.data.frame(alfa.bamboo.c)
alfa.bamboo.c

alfa.bamboo.c.rn <- rownames_to_column(alfa.bamboo.c, (var="Month"))
alfa.bamboo.c.rn$Month <- as.numeric(alfa.bamboo.c.rn$Month)+1
alfa.bamboo.c.rn$Month <- log(alfa.bamboo.c.rn$Month)
glimpse(alfa.bamboo.c.rn)
alfa.bamboo.c.lm <- lm(alfa.bamboo.c~Month, data = alfa.bamboo.c.rn)
summary(alfa.bamboo.c.lm)
#plot(alfa.bamboo.c.lm)
shapiro.test(alfa.bamboo.c.lm$residuals)

# creating alpha and abundance table - Closed --------------------------------------


life_form_df_c <- cbind(alfa.tree.c,alfa.palm.c,alfa.liana.c,
                     alfa.shrub.c,alfa.herb.c,alfa.bamboo.c)

life_form_df_c 

life_form_c <- rownames_to_column(life_form_df_c, (var="Month")) %>% 
  gather(key = "life_form", value = "value", 2:7) %>% 
  rename(fisher_alpha = value) %>% 
  mutate(life_form = recode(life_form, "alfa.tree.c" = "Trees", 
                            "alfa.palm.c" = "Palms",
                            "alfa.liana.c" = "Lianas",
                            "alfa.shrub.c" = "Shrubs",
                            "alfa.herb.c" = "Herbs",
                            "alfa.bamboo.c" = "Bamboos"))

glimpse(life_form_c)
life_form_c 

data.biota.cln.spp.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>% 
  textclean::drop_row("life_form", c("indeterminate")) %>% 
  dplyr::group_by(life_form, Month) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%
  dplyr::ungroup(life_form, Month) %>%
  mutate(life_form = recode(life_form, 
                            "bamboo" = "Bamboos","herb" = "Herbs",
                            "shrub" = "Shrubs","liana" = "Lianas",
                            "tree" = "Trees", "palm" = "Palms")) %>% 
  dplyr::select(Month,life_form,abundances) %>% 
  arrange(match(life_form, c("Trees", "Palms","Lianas",
                       "Shrubs","Herbs","Bamboos")))
  
glimpse(data.biota.cln.spp.c)
data.biota.cln.spp.c



life_form_c$abundances <- data.biota.cln.spp.c$abundances
life_form_c


life.form.ab.alpha.c <- life_form_c %>% 
  mutate(Month = recode(Month, 
                            "0" = 2010, "6" = 2011,
                            "12" = 2011, "18" = 2012, 
                            "24" = 2012, "30" = 2013,
                            "36" = 2013, "42" = 2014,
                            "48" = 2014, "54" = 2015,
                            "60" = 2015, "66" = 2016,
                            "72" = 2016, "78" = 2017,
                            "84" = 2017, "90" = 2018,
                            "96" = 2018))

glimpse(life.form.ab.alpha.c)
life.form.ab.alpha.c

life.form.ab.alpha.c$life_form <- factor(life.form.ab.alpha.c$life_form, levels = c("Trees", "Palms","Lianas",
                                                                                    "Shrubs","Herbs","Bamboos"))
life.form.ab.alpha.c


# grafico fishe's alpha bubble life form - Closed -------------------------------------------------------------------

lf_closed <- ggplot(life.form.ab.alpha.c, aes(x= abundances, y= fisher_alpha, size = Month, 
                  fill = life_form, colour = life_form)) +
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = Month), pch = 16) + 
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
  theme_bw()

lf_closed

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_closed.jpeg", width = 15, height = 17, units = "cm", dpi = 300)


# grafico arrange unlegend --------------------------------------------------------

plot.unlegend.closed <- ggplot(life.form.ab.alpha.c, aes(x= abundances, y= fisher_alpha, size = Month, 
                                         fill = life_form, colour = life_form)) +
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = Month), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12), guide = FALSE) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15), guide = FALSE) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown"), guide = FALSE) +
  labs(y = "Fisher's"~alpha, x = "Abundance", title = "",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw()

plot.unlegend.closed 

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_unlegend_closed.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)


# join graphics --------------------------------------------------------


theme_set(theme_pubr())

ggarrange(
  lf_open, lf_closed + rremove("ylab"),
  ncol = 2, nrow = 1, labels  = "AUTO",
  common.legend = TRUE, legend = "right",
  hjust = c(-4, -2.0), font.label = list(size = 14, color = "black", face =
                      "bold", family = NULL))


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_join.jpeg", width = 25, height = 17, units = "cm", dpi = 1000)


# join graphics with arrenged ---------------------------------------------

plot.lf.open <- unlegend.open + 
  annotation_custom(grob = lf.legend.open, xmin = 120, xmax = 260, ymin = 20, ymax = 38.1)
plot.lf.open

plot.lf.mon.open <- plot.lf.open + 
  annotation_custom(grob = mon.legend.open, xmin = 60, xmax = 110, ymin = 10, ymax = 37)
plot.lf.mon.open

theme_set(theme_pubr())

ggarrange(
  plot.lf.mon.open, plot.unlegend.closed + rremove("ylab"),
  ncol = 2, nrow = 1, labels  = "AUTO",
  common.legend = TRUE, legend = "right",
  hjust = c(-4, -2.0), font.label = list(size = 14, color = "black", face =
                                           "bold", family = NULL))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_join_arranged.jpeg", width = 25, height = 17, units = "cm", dpi = 1000)


# Considering all morpho species same as the life-forms  ------------------------------------------------------------

rm(list = ls())

# Open treatment --------------------------------------------------------

# Trees species abundances - Open ---------------------------------------


data.biota.div.tre.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "tree") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.tre.o

#which(is.na(data.biota.div.tre.o))

data.biota.div.tree.o <- data.biota.div.tre.o %>% 
  pivot_wider(names_from = Species, 
              values_from = abundances)

data.biota.div.tree.o

data.biota.div.tree.o <- data.biota.div.tree.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.tree.o

alfa.tree.o <- fisher.alpha(data.biota.div.tree.o)
alfa.tree.o
summary(alfa.tree.o)

alfa.tree.o <- as.data.frame(alfa.tree.o)
alfa.tree.o

alfa.tree.o.rn <- rownames_to_column(alfa.tree.o, (var="Month"))
alfa.tree.o.rn$Month <- as.numeric(alfa.tree.o.rn$Month)+1
alfa.tree.o.rn$Month <- log(alfa.tree.o.rn$Month)
glimpse(alfa.tree.o.rn)
alfa.tree.o.lm <- lm(alfa.tree.o~Month, data = alfa.tree.o.rn)
summary(alfa.tree.o.lm)
#plot(alfa.tree.o.lm)
shapiro.test(alfa.tree.o.lm$residuals)

# Palms species abundances - Open ---------------------------------------


data.biota.div.pal.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "palm") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.pal.o

#which(is.na(data.biota.div))

data.biota.div.palm.o <- data.biota.div.pal.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.palm.o

data.biota.div.palm.o <- data.biota.div.palm.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.palm.o

alfa.palm.o <- fisher.alpha(data.biota.div.palm.o)
alfa.palm.o
summary(alfa.palm.o)


alfa.palm.o <- as.data.frame(alfa.palm.o)
alfa.palm.o

alfa.palm.o.rn <- rownames_to_column(alfa.palm.o, (var="Month"))
alfa.palm.o.rn$Month <- as.numeric(alfa.palm.o.rn$Month)+1
alfa.palm.o.rn$Month <- log(alfa.palm.o.rn$Month)
glimpse(alfa.palm.o.rn)
alfa.palm.o.lm <- lm(alfa.palm.o~Month, data = alfa.palm.o.rn)
summary(alfa.palm.o.lm)
#plot(alfa.palm.o.lm)
shapiro.test(alfa.palm.o.lm$residuals)


# Lianas species abundances - open ---------------------------------------


data.biota.div.lia.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "liana") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.lia.o

#which(is.na(data.biota.div))

data.biota.div.liana.o <- data.biota.div.lia.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.liana.o

data.biota.div.liana.o <- data.biota.div.liana.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.liana.o

alfa.liana.o <- fisher.alpha(data.biota.div.liana.o)
alfa.liana.o
summary(alfa.liana.o)

alfa.liana.o <- as.data.frame(alfa.liana.o)
alfa.liana.o

alfa.liana.o.rn <- rownames_to_column(alfa.liana.o, (var="Month"))
alfa.liana.o.rn$Month <- as.numeric(alfa.liana.o.rn$Month)+1
alfa.liana.o.rn$Month <- log(alfa.liana.o.rn$Month)
glimpse(alfa.liana.o.rn)
alfa.liana.o.lm <- lm(alfa.liana.o~Month, data = alfa.liana.o.rn)
summary(alfa.liana.o.lm)
#plot(alfa.liana.o.lm)
shapiro.test(alfa.liana.o.lm$residuals)



# Shrubs species abundances - open ---------------------------------------


data.biota.div.shr.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "shrub") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.shr.o

#which(is.na(data.biota.div))

data.biota.div.shrub.o <- data.biota.div.shr.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.shrub.o

data.biota.div.shrub.o <- data.biota.div.shrub.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.shrub.o


alfa.shrub.o <- fisher.alpha(data.biota.div.shrub.o)
alfa.shrub.o
summary(alfa.shrub.o)


alfa.shrub.o <- as.data.frame(alfa.shrub.o)
alfa.shrub.o


alfa.shrub.o.rn <- rownames_to_column(alfa.shrub.o, (var="Month"))
alfa.shrub.o.rn$Month <- as.numeric(alfa.shrub.o.rn$Month)+1
alfa.shrub.o.rn$Month <- log(alfa.shrub.o.rn$Month)
glimpse(alfa.shrub.o.rn)
alfa.shrub.o.lm <- lm(alfa.shrub.o~Month, data = alfa.shrub.o.rn)
summary(alfa.shrub.o.lm)
#plot(alfa.shrub.o.lm)
shapiro.test(alfa.shrub.o.lm$residuals)



# Herbs species abundances - open ---------------------------------------


data.biota.div.her.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "herb") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.her.o

#which(is.na(data.biota.div))

data.biota.div.herb.o <- data.biota.div.her.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.herb.o

data.biota.div.herb.o <- data.biota.div.herb.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.herb.o

alfa.herb.o <- fisher.alpha(data.biota.div.herb.o)
alfa.herb.o
summary(alfa.herb.o)


alfa.herb.o <- as.data.frame(alfa.herb.o)
alfa.herb.o

alfa.herb.o.rn <- rownames_to_column(alfa.herb.o, (var="Month"))
alfa.herb.o.rn$Month <- as.numeric(alfa.herb.o.rn$Month)+1
alfa.herb.o.rn$Month <- log(alfa.herb.o.rn$Month)
glimpse(alfa.herb.o.rn)
alfa.herb.o.lm <- lm(alfa.herb.o~Month, data = alfa.herb.o.rn)
summary(alfa.herb.o.lm)
#plot(alfa.herb.o.lm)
shapiro.test(alfa.herb.o.lm$residuals)


# Bamboos species abundances - open ---------------------------------------


data.biota.div.bam.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open", life_form == "bamboo") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.bam.o


data.biota.div.bamboo.o <- data.biota.div.bam.o %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.bamboo.o

data.biota.div.bamboo.o <- data.biota.div.bamboo.o %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.bamboo.o

alfa.bamboo.o <- fisher.alpha(data.biota.div.bamboo.o)
alfa.bamboo.o
summary(alfa.bamboo.o)

alfa.bamboo.o <- as.data.frame(alfa.bamboo.o)
alfa.bamboo.o

alfa.bamboo.o.rn <- rownames_to_column(alfa.bamboo.o, (var="Month"))
alfa.bamboo.o.rn$Month <- as.numeric(alfa.bamboo.o.rn$Month)+1
alfa.bamboo.o.rn$Month <- log(alfa.bamboo.o.rn$Month)
glimpse(alfa.bamboo.o.rn)
alfa.bamboo.o.lm <- lm(alfa.bamboo.o~Month, data = alfa.bamboo.o.rn)
summary(alfa.bamboo.o.lm)
#plot(alfa.bamboo.o.lm)
shapiro.test(alfa.bamboo.o.lm$residuals)

# creating alpha and abundance table - open --------------------------------------


life_form_df_o <- cbind(alfa.tree.o,alfa.palm.o,alfa.liana.o,
                        alfa.shrub.o,alfa.herb.o,alfa.bamboo.o)

life_form_df_o 

life_form_o <- rownames_to_column(life_form_df_o, (var="Month")) %>% 
  gather(key = "life_form", value = "value", 2:7) %>% 
  rename(fisher_alpha = value) %>% 
  mutate(life_form = recode(life_form, "alfa.tree.o" = "Trees", 
                            "alfa.palm.o" = "Palms",
                            "alfa.liana.o" = "Lianas",
                            "alfa.shrub.o" = "Shrubs",
                            "alfa.herb.o" = "Herbs",
                            "alfa.bamboo.o" = "Bamboos")) %>% 
  mutate(life_form = factor(life_form))

life_form_o

data.biota.oln.spp.o <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "open") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>% 
  textclean::drop_row("life_form", c("indeterminate")) %>% 
  dplyr::group_by(life_form, Month) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%
  dplyr::ungroup(life_form, Month) %>%
  mutate(life_form = recode(life_form, 
                            "bamboo" = "Bamboos","herb" = "Herbs",
                            "shrub" = "Shrubs","liana" = "Lianas",
                            "tree" = "Trees", "palm" = "Palms")) %>% 
  dplyr::select(Month,life_form,abundances) %>% 
  arrange(match(life_form, c("Trees", "Palms","Lianas",
                             "Shrubs","Herbs","Bamboos")))

data.biota.oln.spp.o




life_form_o$abundances <- data.biota.oln.spp.o$abundances
life_form_o


life.form.ab.alpha.o <- life_form_o %>% 
  mutate(Month = recode(Month, 
                        "0" = 2010, "6" = 2011,
                        "12" = 2011, "18" = 2012, 
                        "24" = 2012, "30" = 2013,
                        "36" = 2013, "42" = 2014,
                        "48" = 2014, "54" = 2015,
                        "60" = 2015, "66" = 2016,
                        "72" = 2016, "78" = 2017,
                        "84" = 2017, "90" = 2018,
                        "96" = 2018))

glimpse(life.form.ab.alpha.o)





# grafico fishe's alpha bubble life form - open -------------------------------------------------------------------

ggplot(life.form.ab.alpha.o, aes(x= abundances, y= fisher_alpha, size = Month, 
                                 fill = life_form, colour = life_form)) + 
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = Month), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Fisher's alpha", x = "Abundance", title = "Open",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw()

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_morf_spp_open.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)


# Closed treatment --------------------------------------------------------

# Trees species abundances - Closed ---------------------------------------


data.biota.div.tre.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "tree") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.tre.c

#which(is.na(data.biota.div.tre.c))

data.biota.div.tree.c <- data.biota.div.tre.c %>% 
  pivot_wider(names_from = Species, 
              values_from = abundances)

data.biota.div.tree.c

data.biota.div.tree.c <- data.biota.div.tree.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.tree.c

alfa.tree.c <- fisher.alpha(data.biota.div.tree.c)
alfa.tree.c
summary(alfa.tree.c)

alfa.tree.c <- as.data.frame(alfa.tree.c)
alfa.tree.c

alfa.tree.c.rn <- rownames_to_column(alfa.tree.c, (var="Month"))
alfa.tree.c.rn$Month <- as.numeric(alfa.tree.c.rn$Month)+1
alfa.tree.c.rn$Month <- log(alfa.tree.c.rn$Month)
glimpse(alfa.tree.c.rn)
alfa.tree.c.lm <- lm(alfa.tree.c~Month, data = alfa.tree.c.rn)
summary(alfa.tree.c.lm)
#plot(alfa.tree.c.lm)
shapiro.test(alfa.tree.c.lm$residuals)

# Palms species abundances - Closed ---------------------------------------


data.biota.div.pal.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "palm") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.pal.c

#which(is.na(data.biota.div))

data.biota.div.palm.c <- data.biota.div.pal.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.palm.c

data.biota.div.palm.c <- data.biota.div.palm.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.palm.c

alfa.palm.c <- fisher.alpha(data.biota.div.palm.c)
alfa.palm.c
summary(alfa.palm.c)


alfa.palm.c <- as.data.frame(alfa.palm.c)
alfa.palm.c

alfa.palm.c.rn <- rownames_to_column(alfa.palm.c, (var="Month"))
alfa.palm.c.rn$Month <- as.numeric(alfa.palm.c.rn$Month)+1
alfa.palm.c.rn$Month <- log(alfa.palm.c.rn$Month)
glimpse(alfa.palm.c.rn)
alfa.palm.c.lm <- lm(alfa.palm.c~Month, data = alfa.palm.c.rn)
summary(alfa.palm.c.lm)
#plot(alfa.palm.c.lm)
shapiro.test(alfa.palm.c.lm$residuals)


# Lianas species abundances - Closed ---------------------------------------


data.biota.div.lia.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "liana") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.lia.c

#which(is.na(data.biota.div))

data.biota.div.liana.c <- data.biota.div.lia.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.liana.c

data.biota.div.liana.c <- data.biota.div.liana.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")
data.biota.div.liana.c

alfa.liana.c <- fisher.alpha(data.biota.div.liana.c)
alfa.liana.c
summary(alfa.liana.c)

alfa.liana.c <- as.data.frame(alfa.liana.c)
alfa.liana.c

alfa.liana.c.rn <- rownames_to_column(alfa.liana.c, (var="Month"))
alfa.liana.c.rn$Month <- as.numeric(alfa.liana.c.rn$Month)+1
alfa.liana.c.rn$Month <- log(alfa.liana.c.rn$Month)
glimpse(alfa.liana.c.rn)
alfa.liana.c.lm <- lm(alfa.liana.c~Month, data = alfa.liana.c.rn)
summary(alfa.liana.c.lm)
#plot(alfa.liana.c.lm)
shapiro.test(alfa.liana.c.lm$residuals)



# Shrubs species abundances - Closed ---------------------------------------


data.biota.div.shr.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "shrub") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.shr.c

#which(is.na(data.biota.div))

data.biota.div.shrub.c <- data.biota.div.shr.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.shrub.c

data.biota.div.shrub.c <- data.biota.div.shrub.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.shrub.c


alfa.shrub.c <- fisher.alpha(data.biota.div.shrub.c)
alfa.shrub.c
summary(alfa.shrub.c)


alfa.shrub.c <- as.data.frame(alfa.shrub.c)
alfa.shrub.c


alfa.shrub.c.rn <- rownames_to_column(alfa.shrub.c, (var="Month"))
alfa.shrub.c.rn$Month <- as.numeric(alfa.shrub.c.rn$Month)+1
alfa.shrub.c.rn$Month <- log(alfa.shrub.c.rn$Month)
glimpse(alfa.shrub.c.rn)
alfa.shrub.c.lm <- lm(alfa.shrub.c~Month, data = alfa.shrub.c.rn)
summary(alfa.shrub.c.lm)
#plot(alfa.shrub.c.lm)
shapiro.test(alfa.shrub.c.lm$residuals)



# Herbs species abundances - Closed ---------------------------------------


data.biota.div.her.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "herb") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.her.c

#which(is.na(data.biota.div))

data.biota.div.herb.c <- data.biota.div.her.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.herb.c

data.biota.div.herb.c <- data.biota.div.herb.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.herb.c

alfa.herb.c <- fisher.alpha(data.biota.div.herb.c)
alfa.herb.c
summary(alfa.herb.c)


alfa.herb.c <- as.data.frame(alfa.herb.c)
alfa.herb.c

alfa.herb.c.rn <- rownames_to_column(alfa.herb.c, (var="Month"))
alfa.herb.c.rn$Month <- as.numeric(alfa.herb.c.rn$Month)+1
alfa.herb.c.rn$Month <- log(alfa.herb.c.rn$Month)
glimpse(alfa.herb.c.rn)
alfa.herb.c.lm <- lm(alfa.herb.c~Month, data = alfa.herb.c.rn)
summary(alfa.herb.c.lm)
#plot(alfa.herb.c.lm)
shapiro.test(alfa.herb.c.lm$residuals)


# Bamboos species abundances - Closed ---------------------------------------


data.biota.div.bam.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed", life_form == "bamboo") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Treatment, time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>% 
  dplyr::ungroup(Treatment, time, Species) %>%
  dplyr::select(c(2:4)) %>% 
  mutate_all(replace_na, 0)

data.biota.div.bam.c


data.biota.div.bamboo.c <- data.biota.div.bam.c %>% 
  group_by(Species) %>% 
  pivot_wider(names_from = Species, values_from = abundances)

data.biota.div.bamboo.c

data.biota.div.bamboo.c <- data.biota.div.bamboo.c %>% 
  remove_rownames %>% 
  column_to_rownames(var="time")

data.biota.div.bamboo.c

alfa.bamboo.c <- fisher.alpha(data.biota.div.bamboo.c)
alfa.bamboo.c
summary(alfa.bamboo.c)

alfa.bamboo.c <- as.data.frame(alfa.bamboo.c)
alfa.bamboo.c

alfa.bamboo.c.rn <- rownames_to_column(alfa.bamboo.c, (var="Month"))
alfa.bamboo.c.rn$Month <- as.numeric(alfa.bamboo.c.rn$Month)+1
alfa.bamboo.c.rn$Month <- log(alfa.bamboo.c.rn$Month)
glimpse(alfa.bamboo.c.rn)
alfa.bamboo.c.lm <- lm(alfa.bamboo.c~Month, data = alfa.bamboo.c.rn)
summary(alfa.bamboo.c.lm)
#plot(alfa.bamboo.c.lm)
shapiro.test(alfa.bamboo.c.lm$residuals)

# creating alpha and abundance table - Closed --------------------------------------


life_form_df_c <- cbind(alfa.tree.c,alfa.palm.c,alfa.liana.c,
                        alfa.shrub.c,alfa.herb.c,alfa.bamboo.c)

life_form_df_c 

life_form_c <- rownames_to_column(life_form_df_c, (var="Month")) %>% 
  gather(key = "life_form", value = "value", 2:7) %>% 
  rename(fisher_alpha = value) %>% 
  mutate(life_form = recode(life_form, "alfa.tree.c" = "Trees", 
                            "alfa.palm.c" = "Palms",
                            "alfa.liana.c" = "Lianas",
                            "alfa.shrub.c" = "Shrubs",
                            "alfa.herb.c" = "Herbs",
                            "alfa.bamboo.c" = "Bamboos")) %>% 
  mutate(life_form = factor(life_form))

life_form_c

data.biota.cln.spp.c <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(Treatment == "closed") %>% 
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  textclean::drop_row("Species", c("Indeterminate")) %>% 
  textclean::drop_row("life_form", c("indeterminate")) %>% 
  dplyr::group_by(life_form, Month) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%
  dplyr::ungroup(life_form, Month) %>%
  mutate(life_form = recode(life_form, 
                            "bamboo" = "Bamboos","herb" = "Herbs",
                            "shrub" = "Shrubs","liana" = "Lianas",
                            "tree" = "Trees", "palm" = "Palms")) %>% 
  dplyr::select(Month,life_form,abundances) %>% 
  arrange(match(life_form, c("Trees", "Palms","Lianas",
                             "Shrubs","Herbs","Bamboos")))

data.biota.cln.spp.c




life_form_c$abundances <- data.biota.cln.spp.c$abundances
life_form_c


life.form.ab.alpha.c <- life_form_c %>% 
  mutate(Month = recode(Month, 
                        "0" = 2010, "6" = 2011,
                        "12" = 2011, "18" = 2012, 
                        "24" = 2012, "30" = 2013,
                        "36" = 2013, "42" = 2014,
                        "48" = 2014, "54" = 2015,
                        "60" = 2015, "66" = 2016,
                        "72" = 2016, "78" = 2017,
                        "84" = 2017, "90" = 2018,
                        "96" = 2018))

glimpse(life.form.ab.alpha.c)





# grafico fishe's alpha bubble life form - Closed -------------------------------------------------------------------

lf_closed <- ggplot(life.form.ab.alpha.c, aes(x= abundances, y= fisher_alpha, size = Month, 
                                 fill = life_form, colour = life_form)) + 
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = Month), pch = 16) + 
  scale_size(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Fisher's alpha", x = "Abundance", title = "Closed",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw()

lf_closed

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/diversity/fisher_diversity_lf_morf_spp_closed.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)




.
.
.

.
.
.

######## tabela com dados simulados ----------------------------------------------


dados <- read_csv2("C:/Users/Layu/Desktop/teste.csv")


dados <- dados %>% 
  mutate(Treatment = factor(treatment)) %>% 
  mutate(life_form = factor(life_form)) %>% 
  mutate(life_form = 
           fct_relevel(life_form, "trees", "palms","lianas","shrubs","herbs","bamboos")) %>% 
  mutate(life_form = recode(life_form, "trees" = "Trees", "palms" = "Palms",
                            "lianas" = "Lianas","shrubs" = "Shrubs","herbs" = "Herbs",
                            "bamboos" = "Bamboos"))
dados 

# exploratorio -----------------------------------------------------------


ggplot(dados, aes(x= abundance, y= diversity)) +
  geom_point(aes(size = year, fill = life_form), 
             pch = 21, alpha=0.5, colour = "black") + 
  scale_size(breaks = c(2009:2018), range = c(1,10)) +
  geom_point(aes(size = year, fill = life_form), 
             pch = 1, colour = "black") +
  geom_line(aes(color = life_form), linetype = "solid", size = 1) +
  scale_colour_manual(values = c("springgreen4","yellowgreen",
                                 "khaki4","tan1","sienna3","brown")) +
  geom_point(aes(fill = life_form),shape = 21, size = 3, colour = "black") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  theme_bw()

ggsave("C:/Users/Layu/Desktop/teste.jpeg", width = 15, height = 15, units = "cm", dpi = 300)



# otimizado  --------------------------------------------------------------

?aes
ggplot(dados, aes(x= abundance, y= diversity, size = year, 
                  fill = life_form, colour = life_form)) + 
  geom_point(aes(alpha = year), pch = 21, fill = "grey") + 
  scale_size(breaks = c(2009,2011,2013,2015,2017,2018), range = c(1,10)) +
  scale_alpha_continuous(breaks = c(2009,2011,2013,2015,2017,2018), range = c(0.05,1)) + 
  geom_point(pch = 1, colour = "black") + 
  geom_line(linetype = "solid", size = 1) +  
  scale_colour_manual(values = c("springgreen4","yellowgreen",
                                 "khaki4","tan1","sienna3","brown")) +
  geom_point(shape = 21, size = 3, colour = "none") +   
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  theme_bw()

ggsave("C:/Users/Layu/Desktop/teste2.jpeg", width = 15, height = 15, units = "cm", dpi = 1000)

# final -------------------------------------------------------------------

ggplot(dados, aes(x= abundance, y= diversity, size = year, 
                  fill = life_form, colour = life_form)) + 
  geom_line(linetype = "solid", size = 1) +  
  geom_point(shape = 21, size = 3, colour = "none") + 
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown"), guide = FALSE) +
  geom_point(aes(alpha = year), pch = 16) + 
  scale_size(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(4,12)) +
  scale_alpha_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018), range = c(0.9,0.15)) + 
  geom_point(pch = 1, colour = "black") +
  scale_colour_manual(values = c("springgreen4","yellowgreen","khaki4",
                                 "tan1","sienna3","brown")) +
  labs(y = "Diversity", x = "Abundance", title = "Closed",
       size = "Year", alpha = "Year",colour="Life-forms") +
  theme_bw()

ggsave("C:/Users/Layu/Desktop/teste3.jpeg", width = 15, height = 17, units = "cm", dpi = 1000)








