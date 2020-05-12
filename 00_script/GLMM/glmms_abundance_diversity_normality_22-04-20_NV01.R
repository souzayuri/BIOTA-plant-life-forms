###
#title: Inverse Simpson Diversity
#author: "Yuri Souza"
#data: "15/04/2020"
#content: Create a matriz to calculate inverse simpson diversity
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

#data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
data_biota <- read_csv("life_form_yuri_2019v2.csv")
glimpse(data_biota)



# table with all life forms -------------------------------------------------------------------


bt.spc.mtz <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate") %>% 
  dplyr::select(-c(1,5,23:26,28)) %>% 
  gather(key = "Month", value = "value", 4:20) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
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
  mutate_all(replace_na, 0) %>% 
  mutate(tree.log10 = log10(tree),
         palm.log10 = log10(palm),
         liana.log10 = log10(liana),
         shrub.log10 = log10(shrub),
         herb.log10 = log10(herb),
         bamboo.log10 = log10(bamboo),
         tree.sqrt = sqrt(tree),
         palm.sqrt = sqrt(palm),
         liana.sqrt = sqrt(liana),
         shrub.sqrt = sqrt(shrub),
         herb.sqrt = sqrt(herb),
         bamboo.sqrt = sqrt(bamboo),
         tree.cube = sign(tree)*abs(tree)^(1/3),
         palm.cube = sign(palm)*abs(palm)^(1/3),
         liana.cube = sign(liana)*abs(liana)^(1/3),
         shrub.cube = sign(shrub)*abs(shrub)^(1/3),
         herb.cube = sign(herb)*abs(herb)^(1/3),
         bamboo.cube = sign(bamboo)*abs(bamboo)^(1/3)) %>% 
  hablar::rationalize() %>% 
  mutate_all(replace_na, 0) %>% 
  mutate(Month = as.factor(Month)) %>%
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed"))) %>% 
  mutate(site = Site,
         treatment = Treatment,
         plot = Plot,
         month = Month,
         time = Time) %>%
  unite(site, site, treatment, plot, month, time) %>% rename(PlotID = site) %>% 
  dplyr::select(c(30,1:29))
bt.spc.mtz



# life form abundance table
bt.lf.abn <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate") %>% 
  dplyr::select(-c(1,5,23:26,28)) %>% 
  gather(key = "Month", value = "value", 4:20) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("p", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  mutate(site=Site, treatment = Treatment, plot = Plot, month = Month, time = Time, lifeform = life_form) %>% 
  dplyr::group_by(site, treatment, plot, month, time,life_form) %>%
  summarise(abundances = sum(value)) %>% 
  ungroup(site, treatment, plot, month, time,life_form) %>%
  spread(life_form, abundances) %>% 
  replace(is.na(.), 0) %>% 
  unite(site, site, treatment, plot, month, time) %>% rename(PlotID = site) %>% 
  rename(bamboo.abn = bamboo,
         herb.abn = herb,
         liana.abn = liana,
         palm.abn = palm,
         shrub.abn = shrub,
         tree.abn = tree)
bt.lf.abn



# joins life form abundance table with life form species diversity table


bt.spc.lf.abn.mtz <- left_join(bt.lf.abn, bt.spc.mtz) %>% 
  dplyr::select(c(1,8:36,2:7))
bt.spc.lf.abn.mtz

glimpse(bt.spc.lf.abn.mtz)

# packages normality ------------------------------------------------------



library(rcompanion)
library(MASS)
library(car)
library(moments)
library(bestNormalize)




# trees -------------------------------------------------------------------

# plotNormalHistogram(bt.spc.lf.abn.mtz$tree)
# plotNormalHistogram(bt.spc.lf.abn.mtz$tree.log10)
# plotNormalHistogram(bt.spc.lf.abn.mtz$tree.sqrt)
# plotNormalHistogram(bt.spc.lf.abn.mtz$tree.cube)
# moments::skewness(bt.spc.lf.abn.mtz$tree.sqrt, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$tree.log10, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$tree.sqrt, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$tree.cube, na.rm = TRUE)
# #tree.sqrt melhor valor e hist
# 
# bt.div.sim.tree.glmm <- glmer(tree.sqrt ~ Treatment*log(Time+1) + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.tree.glmm)
# car::Anova(bt.div.sim.tree.glmm, type = "III")
# anova(bt.div.sim.tree.glmm)
# 
# bt.div.sim.abn.tree.glmm <- glmer(tree.sqrt ~ Treatment*tree.abn + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.abn.tree.glmm)
# car::Anova(bt.div.sim.abn.tree.glmm, type = "III")
# anova(bt.div.sim.abn.tree.glmm)

#trees clearly follow a Poisson distribution
plotNormalHistogram(bt.spc.lf.abn.mtz$tree.abn)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(bt.spc.lf.abn.mtz$tree.abn+1))
#so we can run the glmer model with poisson distribution with no problem 
bt.div.sim.tree.glmm.poisson01<- glmer(tree.abn~ Treatment*log(Time+1) + (1 |Site/Month ), family=poisson, data = bt.spc.lf.abn.mtz)
summary(bt.div.sim.tree.glmm.poisson01)


#actually this model has are better random effects accounting for plot pair
#e.g. paired tests by including a third level in the random effects
#and if you look at summary the AIC drops massively
#suggesting that random effects are much better now
bt.div.sim.tree.glmm.poisson02<- glmer(tree.abn~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.spc.lf.abn.mtz)
summary(bt.div.sim.tree.glmm.poisson02)

#now the anova table (type III because we are interested in the interaction)
#first type III anova
car::Anova(bt.div.sim.tree.glmm.poisson02, type = "III")



# palms -------------------------------------------------------------------

# 
# plotNormalHistogram(bt.spc.lf.abn.mtz$palm)
# plotNormalHistogram(bt.spc.lf.abn.mtz$palm.log10)
# plotNormalHistogram(bt.spc.lf.abn.mtz$palm.sqrt)
# plotNormalHistogram(bt.spc.lf.abn.mtz$palm.cube)
# skewness(bt.spc.lf.abn.mtz$palm, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$palm.log10, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$palm.sqrt, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$palm.cube, na.rm = TRUE)
# # palm da o melhor valor e hist
# 
# 
# bt.div.sim.palm.glmm <- glmer(palm ~ Treatment*log(Time+1) + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.palm.glmm)
# car::Anova(bt.div.sim.palm.glmm, type = "III")
# anova(bt.div.sim.palm.glmm)
# 
# bt.div.sim.abn.palm.glmm <- glmer(palm.sqrt ~ Treatment*palm.abn + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.abn.palm.glmm)
# car::Anova(bt.div.sim.abn.palm.glmm, type = "III")
# anova(bt.div.sim.abn.palm.glmm)

#palms seems slightly zero unfalted but when we log it it behaves very well (see below)
plotNormalHistogram(bt.spc.lf.abn.mtz$palm.abn)
#when logged the distribution is almost perfectlly gaussian
plotNormalHistogram(log(bt.spc.lf.abn.mtz$palm.abn+1))
#so we can run the glmer model with poisson distribution with no problem 
bt.div.sim.palm.glmm.poisson02<- glmer(palm.abn~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.spc.lf.abn.mtz)
summary(bt.div.sim.palm.glmm.poisson02)
#and the aova
car::Anova(bt.div.sim.palm.glmm.poisson02, type = "III")


# lianas ------------------------------------------------------------------


# plotNormalHistogram(bt.spc.lf.abn.mtz$liana)
# plotNormalHistogram(bt.spc.lf.abn.mtz$liana.log10)
# plotNormalHistogram(bt.spc.lf.abn.mtz$liana.sqrt)
# plotNormalHistogram(bt.spc.lf.abn.mtz$liana.cube)
# skewness(bt.spc.lf.abn.mtz$liana, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$liana.log10, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$liana.sqrt, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$liana.cube, na.rm = TRUE)
# # liana da melhor valor para liana.sqrt e hist para liana
# 
# bt.div.sim.liana.glmm <- glmer(liana.sqrt ~ Treatment*log(Time+1) + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.liana.glmm)
# car::Anova(bt.div.sim.liana.glmm, type = "III")
# anova(bt.div.sim.liana.glmm)
# 
# 
# bt.div.sim.abn.liana.glmm <- glmer(liana.sqrt ~ Treatment*liana.abn + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.abn.liana.glmm)
# car::Anova(bt.div.sim.abn.liana.glmm, type = "III")
# anova(bt.div.sim.abn.liana.glmm)


#liana seems slightly ZERO INFLATED
plotNormalHistogram(bt.spc.lf.abn.mtz$liana.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.spc.lf.abn.mtz$liana.abn+1))

#let큦 check if negative binomial models deal better with inflation
library(MASS)
library(lmtest)

#first simple glms (Poisson vs negative binomial fit)
poisglm.liana<- glm(liana.abn~ Treatment*log(Time+1), family="poisson", data = bt.spc.lf.abn.mtz)
nbglm.liana<- glm.nb(liana.abn~ Treatment*log(Time+1), data = bt.spc.lf.abn.mtz)
#LRtest shows negative binomial better fit
lrtest(poisglm.liana,nbglm.liana)
summary(nbglm.liana)

#now let큦 compare Poisson with negative binomial GLMMs
poisglmer.liana<- glmer(liana.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.spc.lf.abn.mtz)
nbglmer.liana<- glmer.nb(liana.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.spc.lf.abn.mtz)
#again better fit negative binomial
lrtest(poisglmer.liana,nbglmer.liana)
summary(nbglmer.liana)

#and the aova
car::Anova(nbglmer.liana, type = "III")


# shrubs ------------------------------------------------------------------


# plotNormalHistogram(bt.spc.lf.abn.mtz$shrub)
# plotNormalHistogram(bt.spc.lf.abn.mtz$shrub.log10)
# plotNormalHistogram(bt.spc.lf.abn.mtz$shrub.sqrt)
# plotNormalHistogram(bt.spc.lf.abn.mtz$shrub.cube)
# skewness(bt.spc.lf.abn.mtz$shrub, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$shrub.log10, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$shrub.sqrt, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$shrub.cube, na.rm = TRUE)
# # shrub da melhor valor para shrub.sqrt e hist para shrub
# 
# bt.div.sim.shrub.glmm <- glmer(shrub.sqrt ~ Treatment*log(Time+1) + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.shrub.glmm)
# car::Anova(bt.div.sim.shrub.glmm, type = "III")
# anova(bt.div.sim.shrub.glmm)
# 
# 
# bt.div.sim.abn.shrub.glmm <- glmer(shrub.sqrt ~ Treatment*shrub.abn + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.abn.shrub.glmm)
# car::Anova(bt.div.sim.abn.shrub.glmm, type = "III")
# anova(bt.div.sim.abn.shrub.glmm)

#shrubs seems slightly ZERO INFLATED
plotNormalHistogram(bt.spc.lf.abn.mtz$shrub.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.spc.lf.abn.mtz$shrub.abn+1))

#let큦 check if negative binomial models deal better with inflation
library(MASS)
library(lmtest)

#first simple glms (Poisson vs negative binomial fit)
poisglm.shrub<- glm(shrub.abn~ Treatment*log(Time+1), family="poisson", data = bt.spc.lf.abn.mtz)
nbglm.shrub<- glm.nb(shrub.abn~ Treatment*log(Time+1), data = bt.spc.lf.abn.mtz)
#LRtest shows negative binomial better fit
lrtest(poisglm.shrub,nbglm.shrub)
summary(nbglm.shrub)

#now let큦 compare Poisson with negative binomial GLMMs
poisglmer.shrub<- glmer(shrub.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.spc.lf.abn.mtz)
nbglmer.shrub<- glmer.nb(shrub.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.spc.lf.abn.mtz)
#again better fit negative binomial
#the singular fit in both models is because 
#there큦 almost no temporal variation (month in the random effects)
lrtest(poisglmer.shrub,nbglmer.shrub)
summary(poisglmer.shrub)
summary(nbglmer.shrub)

#and the aova
car::Anova(nbglmer.shrub, type = "III")

# herbs ------------------------------------------------------------------


# plotNormalHistogram(bt.spc.lf.abn.mtz$herb)
# plotNormalHistogram(bt.spc.lf.abn.mtz$herb.log10)
# plotNormalHistogram(bt.spc.lf.abn.mtz$herb.sqrt)
# plotNormalHistogram(bt.spc.lf.abn.mtz$herb.cube)
# skewness(bt.spc.lf.abn.mtz$herb, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$herb.log10, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$herb.sqrt, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$herb.cube, na.rm = TRUE)
# # herb da melhor valor para shrub.log10 e hist tambem
# 
# 
# bt.div.sim.herb.glmm <- glmer(herb.log10 ~ Treatment*log(Time+1) + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.herb.glmm)
# car::Anova(bt.div.sim.herb.glmm, type = "III")
# anova(bt.div.sim.herb.glmm)
# 
# bt.div.sim.abn.herb.glmm <- glmer(herb.sqrt ~ Treatment*herb.abn + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.abn.herb.glmm)
# car::Anova(bt.div.sim.abn.herb.glmm, type = "III")
# anova(bt.div.sim.abn.herb.glmm)

#herbs seems slightly ZERO INFLATED
plotNormalHistogram(bt.spc.lf.abn.mtz$herb.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.spc.lf.abn.mtz$herb.abn+1))

#let큦 check if negative binomial models deal better with inflation
library(MASS)
library(lmtest)

#first simple glms (Poisson vs negative binomial fit)
poisglm.herb<- glm(herb.abn~ Treatment*log(Time+1), family="poisson", data = bt.spc.lf.abn.mtz)
nbglm.herb<- glm.nb(herb.abn~ Treatment*log(Time+1), data = bt.spc.lf.abn.mtz)
#LRtest shows negative binomial better fit
lrtest(poisglm.herb,nbglm.herb)
summary(nbglm.herb)
summary(poisglm.herb)

#now let큦 compare Poisson with negative binomial GLMMs
poisglmer.herb<- glmer(herb.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.spc.lf.abn.mtz)
nbglmer.herb<- glmer.nb(herb.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.spc.lf.abn.mtz)
#again better fit negative binomial
#the singular fit in both models is because 
#there큦 almost no temporal variation (month in the random effects)
lrtest(poisglmer.herb,nbglmer.herb)
summary(poisglmer.herb)
summary(nbglmer.herb)

#and the aova
car::Anova(nbglmer.herb, type = "III")

# bamboos -----------------------------------------------------------------

# 
# 
# plotNormalHistogram(bt.spc.lf.abn.mtz$bamboo)
# plotNormalHistogram(bt.spc.lf.abn.mtz$bamboo.log10)
# plotNormalHistogram(bt.spc.lf.abn.mtz$bamboo.sqrt)
# plotNormalHistogram(bt.spc.lf.abn.mtz$bamboo.cube)
# skewness(bt.spc.lf.abn.mtz$bamboo, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$bamboo.log10, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$bamboo.sqrt, na.rm = TRUE)
# skewness(bt.spc.lf.abn.mtz$bamboo.cube, na.rm = TRUE)
# # bamboo da melhor valor para cube e visual tbm
# 
# 
# bt.div.sim.bamboo.glmm <- glmer(bamboo.cube ~ Treatment*log(Time+1) + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.bamboo.glmm)
# car::Anova(bt.div.sim.bamboo.glmm, type = "III")
# anova(bt.div.sim.bamboo.glmm)
# 
# 
# bt.div.sim.abn.bamboo.glmm <- glmer(bamboo.sqrt ~ Treatment*bamboo.abn + (1 |Site/Month ), family=gaussian, data = bt.spc.lf.abn.mtz)
# summary(bt.div.sim.abn.bamboo.glmm)
# car::Anova(bt.div.sim.abn.bamboo.glmm, type = "III")
# anova(bt.div.sim.abn.bamboo.glmm)

#bamboos seems slightly ZERO INFLATED
plotNormalHistogram(bt.spc.lf.abn.mtz$bamboo.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.spc.lf.abn.mtz$bamboo.abn+1))

#let큦 check if negative binomial models deal better with inflation
library(MASS)
library(lmtest)

#first simple glms (Poisson vs negative binomial fit)
poisglm.bamboo<- glm(bamboo.abn~ Treatment*log(Time+1), family="poisson", data = bt.spc.lf.abn.mtz)
nbglm.bamboo<- glm.nb(bamboo.abn~ Treatment*log(Time+1), data = bt.spc.lf.abn.mtz)
#LRtest shows negative binomial better fit
lrtest(poisglm.bamboo,nbglm.bamboo)
summary(nbglm.bamboo)
summary(poisglm.bamboo)

#now let큦 compare Poisson with negative binomial GLMMs
poisglmer.bamboo<- glmer(bamboo.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.spc.lf.abn.mtz)
nbglmer.bamboo<- glmer.nb(bamboo.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.spc.lf.abn.mtz)
#again better fit negative binomial
#the singular fit in both models is because 
#there큦 almost no temporal variation (month in the random effects)
lrtest(poisglmer.bamboo,nbglmer.bamboo)
summary(poisglmer.bamboo)
summary(nbglmer.bamboo)

#and the aova
car::Anova(nbglmer.bamboo, type = "III")




#### outras formas de testar normalidade. 
####Nenhuma delas transformou os dados para uma distribui챌찾o mais proxima da normal.

# 1)
#x <- bestNormalize::bestNormalize(bt.spc.lf.abn.mtz$tree, standardize = TRUE, allow_orderNorm = TRUE,
#              allow_lambert_s = FALSE, allow_lambert_h = FALSE, allow_exp = TRUE,
#              out_of_sample = TRUE, cluster = NULL, k = 10, r = 10,
#              loo = FALSE, warn = TRUE, quiet = FALSE, tr_opts = list())
#x
#MASS::truehist(x$x.t)

# 2) 
#T_tuk = rcompanion::transformTukey(bt.spc.lf.abn.mtz$tree, plotit=FALSE)
