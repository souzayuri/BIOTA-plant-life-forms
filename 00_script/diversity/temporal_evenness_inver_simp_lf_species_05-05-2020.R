###
#title: Inverse Simpson eveness
#author: "Yuri Souza"
#data: "05/05/2020"
#content: Create a life-form species matriz to calculate inverse simpson diversity to apply to pielous eveness
###

rm(list = ls())



# load packages and table ----------------------------------------------------------


library(tidyverse)
library(hillR)
library(hablar)
library(stringr)
library(lme4) 
library(car)
library(naniar)
library(ggpubr)
library(grid)
library(codyn)
library(rcompanion)
library(MASS)
library(lmtest)



data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)


# table with inverse simpsons calculated  -------------------------------------------------------------------


bt.invsim.mtz <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4, 24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  mutate(site=Site, treatment = Treatment, plot = Plot, month = Month, time = Time, lifeform = life_form) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time, life_form) %>% rename(PlotID = Site) %>% 
  dplyr::group_by(PlotID, site, treatment, plot, month, time, Species) %>%
  summarise(abundances = sum(value)) %>% 
  ungroup(PlotID, site, treatment, plot, month, time, species) %>%
  spread(Species, abundances) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  dplyr::select(-c(2:6)) %>% 
  column_to_rownames(var="PlotID") %>% 
  hillR::hill_taxa(q = 2, MARGIN = 1) %>% 
  as.data.frame() %>% rownames_to_column(var="PlotID") %>% 
  separate(PlotID, c("Site", "Treatment", "Plot", "Month", "Time", "Life_form"), convert = TRUE) %>% 
  rename(simp.inv.div = ".") %>% 
  hablar::rationalize() %>% 
  spread(Life_form, simp.inv.div) %>%
  hablar::rationalize() %>% 
  mutate(Month = as.factor(Month)) %>%
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed"))) %>% 
  mutate(site = Site,
         treatment = Treatment,
         plot = Plot,
         month = Month,
         time = Time) %>%
  unite(site, site, treatment, plot, month, time) %>% rename(PlotID = site) %>% 
  rename(bamboo.invs = bamboo,
         herb.invs = herb,
         liana.invs = liana,
         palm.invs = palm,
         shrub.invs = shrub,
         tree.invs = tree)
bt.invsim.mtz



# total abundance table  -------------------------------------------------------------------


bt.abu.mtz <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, life_form) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>% 
  dplyr::mutate(site = Site, plot = Plot, month = Month,
                time = Time, treatment = Treatment) %>%
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(PlotID, site, plot, month,
                time, treatment, abundances,life_form) %>% 
  spread(life_form, abundances) %>% 
  rename(bamboo.abn = bamboo,
         herb.abn = herb,
         liana.abn = liana,
         palm.abn = palm,
         shrub.abn = shrub,
         tree.abn = tree) %>% 
  select(1,7:12)

bt.abu.mtz



# life form total richness matriz -----------------------------------------
# tree -------------------------------------------------------------------


bt.rich.tree <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "tree") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Site, Plot, Treatment, Month, Time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Month", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  mutate(life_form = rep("trees")) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(1,2) %>% 
  rename(tree.rich = richness)

bt.rich.tree 





# liana -------------------------------------------------------------------


bt.rich.liana <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "liana") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Site, Plot, Treatment, Month, Time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Month", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  mutate(life_form = rep("lianas")) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(1,2) %>% 
  rename(liana.rich = richness)

bt.rich.liana 



# palm -------------------------------------------------------------------


bt.rich.palm <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "palm") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Site, Plot, Treatment, Month, Time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Month", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  mutate(life_form = rep("palms")) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(1,2) %>% 
  rename(palm.rich = richness)

bt.rich.palm 



# shrub -------------------------------------------------------------------


bt.rich.shrub <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "shrub") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Site, Plot, Treatment, Month, Time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Month", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  mutate(life_form = rep("shrubs")) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(1,2) %>% 
  rename(shrub.rich = richness)

bt.rich.shrub 



# herb -------------------------------------------------------------------


bt.rich.herb <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "herb") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Site, Plot, Treatment, Month, Time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Month", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  mutate(life_form = rep("herbs")) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(1,2) %>% 
  rename(herb.rich = richness)

bt.rich.herb 



# bamboo -------------------------------------------------------------------

bt.rich.bamboo <- data_biota %>% 
  dplyr::select(-c(4, 24:26,28)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:22) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  rename(life_form = `Life Form`) %>% 
  filter(life_form == "bamboo") %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, Time, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::ungroup(Site, Plot, Treatment, Month, Time, Species) %>%
  mutate_all(replace_na, 0) %>% 
  codyn::community_structure(time.var=c("Site", "Plot", "Month", "Time"),
                             replicate.var = "Treatment",
                             abundance.var = "abundances",
                             metric = "SimpsonEvenness") %>% # for Simpson's evenness measure
  mutate(life_form = rep("bamboos")) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::select(1,2) %>% 
  rename(bamboo.rich = richness)

bt.rich.bamboo 



# join richness tables -------------------------------------------------------------


bt.rich <-left_join(bt.rich.tree, bt.rich.liana, by = "PlotID") %>% 
  left_join(bt.rich.palm, by = "PlotID") %>% left_join(bt.rich.shrub, by = "PlotID") %>% 
  left_join(bt.rich.herb, by = "PlotID") %>% left_join(bt.rich.bamboo, by = "PlotID")
bt.rich



  select(1,4,8,5,6,7,2,9) %>%
  mutate(site = as.factor(site),
         treatment = as.factor(treatment),
         month = as.factor(month)) %>% 
  rename(bamboos.rich = richness,
         bamboos.abun = abundances)
data.biota.abu.rich.bamboo





  mutate_all(replace_na, 0) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

glimpse(total.lf.abu.div)

total.lf.abu.div

# join inverse simpsons, abundances and richness table --------------------

bt.invsim.abu.rich.mtz <-left_join(bt.invsim.mtz, bt.abu.mtz, by = "PlotID") %>% 
  left_join(bt.rich, by = "PlotID") %>% 
  select(12, 1:5, 18,15,16,17,14,13,19:24,11,8,9,10,7,6) %>% 
  hablar::rationalize() %>%
  mutate_all(replace_na, 0) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x)) %>% 
  hablar::rationalize() %>%
  mutate_all(replace_na, 0)


bt.invsim.abu.rich.mtz


# eveness table -----------------------------------------------------------


bt.invsim.abu.rich.mtz.even <- bt.invsim.abu.rich.mtz %>% 
  mutate(tree.even = tree.invs/log(tree.rich),
         liana.even = liana.invs/log(liana.rich),
         palm.even = palm.invs/log(palm.rich),
         shrub.even = shrub.invs/log(shrub.rich),
         herb.even = herb.invs/log(herb.rich),
         bamboo.even = bamboo.invs/log(bamboo.rich)) %>% 
  hablar::rationalize() %>%
  mutate_all(replace_na, 0) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

bt.invsim.abu.rich.mtz.even




# glmm life-form species evenness -----------------------------------------


#palms

#palms seems slightly zero unfalted but when we log it it behaves very well (see below)
plotNormalHistogram(bt.invsim.abu.rich.mtz.even$palm.invs)
#when logged the distribution is almost perfectlly gaussian
plotNormalHistogram(log(bt.invsim.abu.rich.mtz.even$palm.invs^3))
#so we can run the glmer model with poisson distribution with no problem 
bt.inv.sim.palm.glmm<- glmer.nb(palm.invs~ Treatment*palm.abn + (1 |Site/Plot/Month), data = bt.invsim.abu.rich.mtz.even)
summary(bt.inv.sim.palm.glmm)
#and the aova
car::Anova(bt.inv.sim.palm.glmm, type = "III")

# modelo sem interação para a anova

mo1lf.palm <- glmer(palm.abn ~ (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.palm)
# anova para testar a significancia da interação do modelo
anova(bt.div.sim.palm.glmm.poisson02,mo1lf.palm)


bt.invsim.abu.rich.mtz.even

