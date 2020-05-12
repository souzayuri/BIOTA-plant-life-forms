# Informations ------------------------------------------------------------

### title: Temporal evenness diversity  ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 30/03/2020
### Description: this script calculates the species svenness by each plot 

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

data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv", locale = locale(encoding = "ASCII")) # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)



# trees -------------------------------------------------------------------

# diversity table

data.biota.div.tree <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "tree") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Plot, Treatment, time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Time"),
                           replicate.var = "Treatment",
                           abundance.var = "abundances",
                           metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  dplyr::group_by(Site, Plot, Time, Treatment) %>% 
  mutate(pilou_evenness = SimpsonEvenness/log(richness)) %>% 
  mutate(life_form = rep("trees")) %>% 
  dplyr::ungroup(Site, Plot, Time, Treatment) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, richness, 
                SimpsonEvenness, pilou_evenness,life_form) %>% 
  mutate(site = as.character(site),
         treatment = as.character(treatment),
         life_form = as.character(life_form))


glimpse(data.biota.div.tree)


# abundance table 

data.biota.abu.tree <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "tree") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Time, Treatment) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, abundances) 
  
glimpse(data.biota.abu.tree)


# join tables 

data.biota.abu.div.tree <-left_join(data.biota.div.tree, data.biota.abu.tree) %>% 
  select(1:6,10,7:9) %>%
  mutate(site = as.factor(site),
         treatment = as.factor(treatment),
         life_form = as.factor(life_form))
data.biota.abu.div.tree


#shapiro.test(data.biota.abu.div.tree$pilou_evenness)
#hist(log(data.biota.abu.div.tree$pilou_evenness))
#ggdensity(log(data.biota.abu.div.tree$pilou_evenness))
#ggqqplot(log(data.biota.abu.div.tree$pilou_evenness))



# lianas -------------------------------------------------------------------


# diversity table

data.biota.div.liana <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "liana") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Plot, Treatment, time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  dplyr::group_by(Site, Plot, Time, Treatment) %>% 
  mutate(pilou_evenness = SimpsonEvenness/log(richness)) %>% 
  mutate(life_form = rep("lianas")) %>% 
  dplyr::ungroup(Site, Plot, Time, Treatment) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, richness, 
                SimpsonEvenness, pilou_evenness,life_form) %>% 
  mutate(site = as.character(site),
         treatment = as.character(treatment),
         life_form = as.character(life_form))


glimpse(data.biota.div.liana)


# abundance table 

data.biota.abu.liana <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "liana") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Time, Treatment) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, abundances) 

glimpse(data.biota.abu.liana)


# join tables 

data.biota.abu.div.liana <-left_join(data.biota.div.liana, data.biota.abu.liana) %>% 
  select(1:6,10,7:9) %>%
  mutate(site = as.factor(site),
         treatment = as.factor(treatment),
         life_form = as.factor(life_form))
data.biota.abu.div.liana


#shapiro.test(data.biota.abu.div.liana$pilou_evenness)
#hist(log(data.biota.abu.div.liana$pilou_evenness))
#ggdensity(log(data.biota.abu.div.liana$pilou_evenness))
#ggqqplot(log(data.biota.abu.div.liana$pilou_evenness))


# palms -------------------------------------------------------------------


# diversity table

data.biota.div.palm <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "palm") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Plot, Treatment, time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  dplyr::group_by(Site, Plot, Time, Treatment) %>% 
  mutate(pilou_evenness = SimpsonEvenness/log(richness)) %>% 
  mutate(life_form = rep("palms")) %>% 
  dplyr::ungroup(Site, Plot, Time, Treatment) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, richness, 
                SimpsonEvenness, pilou_evenness,life_form) %>% 
  mutate(site = as.character(site),
         treatment = as.character(treatment),
         life_form = as.character(life_form))


glimpse(data.biota.div.palm)


# abundance table 

data.biota.abu.palm <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "palm") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Time, Treatment) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, abundances) 

glimpse(data.biota.abu.palm)


# join tables 

data.biota.abu.div.palm <-left_join(data.biota.div.palm, data.biota.abu.palm) %>% 
  select(1:6,10,7:9) %>%
  mutate(site = as.factor(site),
         treatment = as.factor(treatment),
         life_form = as.factor(life_form))
data.biota.abu.div.palm


#shapiro.test(data.biota.abu.div.palm$pilou_evenness)
#hist(log(data.biota.abu.div.palm$pilou_evenness))
#ggdensity(log(data.biota.abu.div.palm$pilou_evenness))
#ggqqplot(log(data.biota.abu.div.palm$pilou_evenness))


# shrubs -------------------------------------------------------------------


# diversity table

data.biota.div.shrub <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "shrub") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Plot, Treatment, time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  dplyr::group_by(Site, Plot, Time, Treatment) %>% 
  mutate(pilou_evenness = SimpsonEvenness/log(richness)) %>% 
  mutate(life_form = rep("shrubs")) %>% 
  dplyr::ungroup(Site, Plot, Time, Treatment) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, richness, 
                SimpsonEvenness, pilou_evenness,life_form) %>% 
  mutate(site = as.character(site),
         treatment = as.character(treatment),
         life_form = as.character(life_form))


glimpse(data.biota.div.shrub)


# abundance table 

data.biota.abu.shrub <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "shrub") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Time, Treatment) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, abundances) 

glimpse(data.biota.abu.shrub)


# join tables 

data.biota.abu.div.shrub <-left_join(data.biota.div.shrub, data.biota.abu.shrub) %>% 
  select(1:6,10,7:9) %>%
  mutate(site = as.factor(site),
         treatment = as.factor(treatment),
         life_form = as.factor(life_form))
data.biota.abu.div.shrub


#shapiro.test(data.biota.abu.div.shrub$pilou_evenness)
#hist(log(data.biota.abu.div.shrub$pilou_evenness))
#ggdensity(log(data.biota.abu.div.shrub$pilou_evenness))
#ggqqplot(log(data.biota.abu.div.shrub$pilou_evenness))

# herbs -------------------------------------------------------------------



# diversity table

data.biota.div.herb <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "herb") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Plot, Treatment, time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  dplyr::group_by(Site, Plot, Time, Treatment) %>% 
  mutate(pilou_evenness = SimpsonEvenness/log(richness)) %>% 
  mutate(life_form = rep("herbs")) %>% 
  dplyr::ungroup(Site, Plot, Time, Treatment) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, richness, 
                SimpsonEvenness, pilou_evenness,life_form) %>% 
  mutate(site = as.character(site),
         treatment = as.character(treatment),
         life_form = as.character(life_form))


glimpse(data.biota.div.herb)


# abundance table 

data.biota.abu.herb <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "herb") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Time, Treatment) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, abundances) 

glimpse(data.biota.abu.herb)


# join tables 

data.biota.abu.div.herb <-left_join(data.biota.div.herb, data.biota.abu.herb) %>% 
  select(1:6,10,7:9) %>%
  mutate(site = as.factor(site),
         treatment = as.factor(treatment),
         life_form = as.factor(life_form))
data.biota.abu.div.herb


#shapiro.test(data.biota.abu.div.herb$pilou_evenness)
#hist(log(data.biota.abu.div.herb$pilou_evenness))
#ggdensity(log(data.biota.abu.div.herb$pilou_evenness))
#ggqqplot(log(data.biota.abu.div.herb$pilou_evenness))


# bamboos -------------------------------------------------------------------


data.biota.div.bamboo <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "bamboo") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Plot, Treatment, time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  dplyr::group_by(Site, Plot, Time, Treatment) %>% 
  mutate(pilou_evenness = SimpsonEvenness/log(richness)) %>% 
  mutate(life_form = rep("bamboos")) %>% 
  dplyr::ungroup(Site, Plot, Time, Treatment) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, richness, 
                SimpsonEvenness, pilou_evenness,life_form) %>% 
  mutate(site = as.character(site),
         treatment = as.character(treatment),
         life_form = as.character(life_form))


glimpse(data.biota.div.bamboo)


# abundance table 

data.biota.abu.bamboo <- data_biota %>% 
  dplyr::select(-c(1,5,23:26)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "bamboo") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::group_by(Site, Plot, Time, Treatment) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, 
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Plot, Time, Treatment) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, 
                time, treatment, abundances) 

glimpse(data.biota.abu.bamboo)


# join tables 

data.biota.abu.div.bamboo <-left_join(data.biota.div.bamboo, data.biota.abu.bamboo) %>% 
  select(1:6,10,7:9) %>%
  mutate(site = as.factor(site),
         treatment = as.factor(treatment),
         life_form = as.factor(life_form))
data.biota.abu.div.bamboo


#shapiro.test(data.biota.abu.div.bamboo$pilou_evenness)
#hist(log(data.biota.abu.div.bamboo$pilou_evenness))
#ggdensity(log(data.biota.abu.div.bamboo$pilou_evenness))
#ggqqplot(log(data.biota.abu.div.bamboo$pilou_evenness))


# bind df -----------------------------------------------------------------

total.lf.abu.div <- bind_rows(data.biota.abu.div.tree,
                             data.biota.abu.div.liana,
                             data.biota.abu.div.palm,
                             data.biota.abu.div.shrub,
                             data.biota.abu.div.herb,
                             data.biota.abu.div.bamboo) %>% 
  mutate(site = as.factor(site),
         life_form = as.factor(life_form)) %>% 
  mutate_all(replace_na, 0) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

glimpse(total.lf.abu.div)

total.lf.abu.div

#shapiro.test(total.lf.divers$pilou_evenness)
#hist(log(total.lf.divers$pilou_evenness))
#ggdensity(log(total.lf.divers$pilou_evenness))
#ggqqplot(log(total.lf.divers$pilou_evenness))

write_csv(total.lf.abu.div, "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/diversity/total.lf.diversity_31-03-20.csv")

sample_n(total.lf.abu.div, size = 10)
