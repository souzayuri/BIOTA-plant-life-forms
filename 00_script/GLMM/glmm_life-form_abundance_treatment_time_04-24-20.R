
###
#title: Life form abundance Glmm
#author: "Yuri Souza"
#data: "24/04/2020"
#content: Create a life form abundance table and use it in glmm models. This script also test the best normality for the data and best glmm option for it, suggested and commented by Nachoman.
###

rm(list = ls())



# load packages and table ----------------------------------------------------------


library(tidyverse)
library(rcompanion)
library(stringr)
library(MASS)
library(lmtest)
library(lme4) 
library(car)


#data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.csv")
glimpse(data_biota)



# life form abundance table
bt.lf.abn <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4,24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" ", ".")) %>% 
  dplyr::group_by(Site, Treatment, Plot, Month, Time, life_form) %>%
  summarise(Abundances = sum(value)) %>% 
  ungroup(Site, Treatment, Plot, Month, Time,life_form) %>%
  spread(life_form, Abundances) %>% 
  replace(is.na(.), 0) %>% 
  rename(bamboo.abn = bamboo,
         herb.abn = herb,
         liana.abn = liana,
         palm.abn = palm,
         shrub.abn = shrub,
         tree.abn = tree) %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed")))
bt.lf.abn


# GLMM abundance ----------------------------------------------------------



# trees -------------------------------------------------------------------

#trees clearly follow a Poisson distribution
plotNormalHistogram(bt.lf.abn$tree.abn)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(bt.lf.abn$tree.abn+1))
#so we can run the glmer model with poisson distribution with no problem 
bt.div.sim.tree.glmm.poisson01<- glmer(tree.abn~ Treatment*log(Time+1) + (1 |Site/Month ), family=poisson, data = bt.lf.abn)
summary(bt.div.sim.tree.glmm.poisson01)


#actually this model has are better random effects accounting for plot pair
#e.g. paired tests by including a third level in the random effects
#and if you look at summary the AIC drops massively
#suggesting that random effects are much better now
bt.div.sim.tree.glmm.poisson02<- glmer(tree.abn~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(bt.div.sim.tree.glmm.poisson02)

#now the anova table (type III because we are interested in the interaction)
#first type III anova
car::Anova(bt.div.sim.tree.glmm.poisson02, type = "III")

# modelo sem interação para a anova

mo1lf.tree <- glmer(tree.abn ~ (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.tree)
# anova para testar a significancia da interação do modelo
anova(bt.div.sim.tree.glmm.poisson02,mo1lf.tree)

# palms -------------------------------------------------------------------


#palms seems slightly zero unfalted but when we log it it behaves very well (see below)
plotNormalHistogram(bt.lf.abn$palm.abn)
#when logged the distribution is almost perfectlly gaussian
plotNormalHistogram(log(bt.lf.abn$palm.abn+1))
#so we can run the glmer model with poisson distribution with no problem 
bt.div.sim.palm.glmm.poisson02<- glmer(palm.abn~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(bt.div.sim.palm.glmm.poisson02)
#and the aova
car::Anova(bt.div.sim.palm.glmm.poisson02, type = "III")

# modelo sem interação para a anova

mo1lf.palm <- glmer(palm.abn ~ (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.palm)
# anova para testar a significancia da interação do modelo
anova(bt.div.sim.palm.glmm.poisson02,mo1lf.palm)


# lianas ------------------------------------------------------------------


#liana seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$liana.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$liana.abn+1))

#let?s check if negative binomial models deal better with inflation
#library(MASS)
#library(lmtest)

#first simple glms (Poisson vs negative binomial fit)
poisglm.liana<- glm(liana.abn~ Treatment*log(Time+1), family="poisson", data = bt.lf.abn)
poisglm.liana
nbglm.liana<- glm.nb(liana.abn~ Treatment*log(Time+1), data = bt.lf.abn)
nbglm.liana
#LRtest shows negative binomial better fit
lrtest(poisglm.liana,nbglm.liana)
summary(nbglm.liana)

#now let?s compare Poisson with negative binomial GLMMs
poisglmer.liana<- glmer(liana.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.lf.abn)
summary(poisglmer.liana)
nbglmer.liana<- glmer.nb(liana.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.liana)

#again better fit negative binomial
lrtest(poisglmer.liana,nbglmer.liana)
summary(nbglmer.liana)

#and the aova
car::Anova(nbglmer.liana, type = "III")

# modelo sem interação para a anova

mo1lf.liana <- glmer(liana.abn ~ (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.liana)
# anova para testar a significancia da interação do modelo
anova(nbglmer.liana,mo1lf.liana)



# shrubs ------------------------------------------------------------------


#shrubs seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$shrub.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$shrub.abn+1))

#let?s check if negative binomial models deal better with inflation

#first simple glms (Poisson vs negative binomial fit)
poisglm.shrub<- glm(shrub.abn~ Treatment*log(Time+1), family="poisson", data = bt.lf.abn)
summary(poisglm.shrub)
nbglm.shrub<- glm.nb(shrub.abn~ Treatment*log(Time+1), data = bt.lf.abn)
summary(nbglm.shrub)
#LRtest shows negative binomial better fit
lrtest(poisglm.shrub,nbglm.shrub)
summary(nbglm.shrub)

#now let?s compare Poisson with negative binomial GLMMs
poisglmer.shrub<- glmer(shrub.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.lf.abn)
summary(poisglmer.shrub)
nbglmer.shrub<- glmer.nb(shrub.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.shrub)


#again better fit negative binomial
#the singular fit in both models is because 
#there?s almost no temporal variation (month in the random effects)
lrtest(poisglmer.shrub,nbglmer.shrub)

#and the aova
car::Anova(nbglmer.shrub, type = "III")

# modelo sem interação para a anova

mo1lf.shrub <- glmer(shrub.abn ~ (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.shrub)
# anova para testar a significancia da interação do modelo
anova(nbglmer.shrub,mo1lf.shrub)


# herbs ------------------------------------------------------------------

#herbs seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$herb.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$herb.abn+1))


#first simple glms (Poisson vs negative binomial fit)
poisglm.herb<- glm(herb.abn~ Treatment*log(Time+1), family="poisson", data = bt.lf.abn)
summary(poisglm.herb)
nbglm.herb<- glm.nb(herb.abn~ Treatment*log(Time+1), data = bt.lf.abn)
summary(nbglm.herb)
#LRtest shows negative binomial better fit
lrtest(poisglm.herb,nbglm.herb)



#now let?s compare Poisson with negative binomial GLMMs
poisglmer.herb<- glmer(herb.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.lf.abn)
summary(poisglmer.herb)
nbglmer.herb<- glmer.nb(herb.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.herb)

#again better fit negative binomial
#the singular fit in both models is because 
#there?s almost no temporal variation (month in the random effects)
lrtest(poisglmer.herb,nbglmer.herb)
summary(poisglmer.herb)
summary(nbglmer.herb)

#and the aova
car::Anova(nbglmer.herb, type = "III")


# modelo sem interação para a anova

mo1lf.herb <- glmer(herb.abn ~ (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.herb)
# anova para testar a significancia da interação do modelo
anova(nbglmer.herb,mo1lf.herb)




# bamboos -----------------------------------------------------------------


#bamboos seems slightly ZERO INFLATED
plotNormalHistogram(bt.lf.abn$bamboo.abn)
#when logged the distribution remains ZERO INFLATED
plotNormalHistogram(log(bt.lf.abn$bamboo.abn+1))

#first simple glms (Poisson vs negative binomial fit)
poisglm.bamboo<- glm(bamboo.abn~ Treatment*log(Time+1), family="poisson", data = bt.lf.abn)
summary(poisglm.bamboo)
nbglm.bamboo<- glm.nb(bamboo.abn~ Treatment*log(Time+1), data = bt.lf.abn)
summary(nbglm.bamboo)
#LRtest shows negative binomial better fit
lrtest(poisglm.bamboo,nbglm.bamboo)



#now let?s compare Poisson with negative binomial GLMMs
poisglmer.bamboo<- glmer(bamboo.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), family="poisson", data = bt.lf.abn)
summary(poisglmer.bamboo)
nbglmer.bamboo<- glmer.nb(bamboo.abn~ Treatment*log(Time+1)+ (1 |Site/Plot/Month), data = bt.lf.abn)
summary(nbglmer.bamboo)
#again better fit negative binomial
#the singular fit in both models is because 
#there?s almost no temporal variation (month in the random effects)
lrtest(poisglmer.bamboo,nbglmer.bamboo)

#and the aova
car::Anova(nbglmer.bamboo, type = "III")


# modelo sem interação para a anova

mo1lf.bamboo <- glmer(bamboo.abn ~ (1 |Site/Plot/Month), family=poisson, data = bt.lf.abn)
summary(mo1lf.bamboo)
# anova para testar a significancia da interação do modelo
anova(nbglmer.bamboo,mo1lf.bamboo)

