### title: GLMM - Relative abundance ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 04/04/2020
### Description: Create a table to test the life-form realtive abundance using GLMM


# loading packages and tables ---------------------------------------------



rm(list = ls()) 

library(tidyverse)
library(rcompanion)
library(stringr)
library(MASS)
library(lmtest)
library(lme4) 
library(car)
library(glmmTMB) ## zero inflation
library(bbmle) ## for AICtab


data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.csv")
glimpse(data_biota)


lf.rel.abun <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4,24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% # copia a coluna de tempo e transforma em numerico
  group_by(Site, Plot, Treatment, Time, Month, life_form) %>% 
  summarise(Abundance = sum(value)) %>%
  #mutate(Abundance = Abundance / sum(Abundance)) %>% 
  spread(life_form, Abundance) %>%  #converte para widescreen+
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed"))) %>% 
  mutate(nontrees = rowSums(.[6:10])) %>% 
  mutate(nonpalms = rowSums(.[c(6:8,10:11)])) %>% 
  mutate(nonlianas = rowSums(.[c(6:7,9:11)])) %>% 
  mutate(nonshrubs = rowSums(.[c(6:9,11)])) %>% 
  mutate(nonherbs = rowSums(.[c(6,8:11)])) %>% 
  mutate(nonbamboos = rowSums(.[c(7:11)]))
lf.rel.abun



# trees -------------------------------------------------------------------



plotNormalHistogram(lf.rel.abun$tree)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(lf.rel.abun$tree+1))


mo1lf.tree.time <- glmer(cbind(tree,nontrees) ~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family=binomial, data = lf.rel.abun)
summary(mo1lf.tree.time)
car::Anova(mo1lf.tree.time, type = "III", test.statistic="Chisq")

mo1lf.tree <- glmer(cbind(tree,nontrees) ~ (1 |Site/Plot/Month), family=binomial, data = lf.rel.abun)
summary(mo1lf.tree)
car::Anova(mo1lf.tree, type = "III", test.statistic="Chisq")

anova(mo1lf.tree.time,mo1lf.tree)


# palms -------------------------------------------------------------------

plotNormalHistogram(lf.rel.abun$palm)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(lf.rel.abun$palm+1))

mo1lf.palm.time <- glmer(cbind(palm,nonpalms) ~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family = binomial, data = lf.rel.abun)
summary(mo1lf.palm.time)
car::Anova(mo1lf.palm.time, type = "III", test.statistic="Chisq")

mo1lf.palm <- glmer(cbind(palm,nonpalms) ~ (1 |Site/Plot/Month), family=binomial, data = lf.rel.abun)
summary(mo1lf.palm)
car::Anova(mo1lf.palm, type = "III", test.statistic="Chisq")

anova(mo1lf.palm.time,mo1lf.palm)


# lianas ------------------------------------------------------------------


plotNormalHistogram(lf.rel.abun$liana)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(lf.rel.abun$liana+1))


# GLM
#first simple glms (Poisson vs negative binomial fit)
#poisglm.rel.abn.liana<- glm(liana~ Treatment*log(Time+1), family="poisson", data = lf.rel.abun)
#poisglm.rel.abn.liana

#nbglm.rel.abn.liana<- glm.nb(liana~ Treatment*log(Time+1), data = lf.rel.abun)
#nbglm.rel.abn.liana
#LRtest shows negative binomial better fit
#lrtest(poisglm.rel.abn.liana,nbglm.rel.abn.liana)
#summary(nbglm.rel.abn.liana)


# GLMM
#now let?s compare binomial with negative binomial GLMMs

mo1lf.liana.time.rel.abu <- glmer(cbind(liana,nonlianas) ~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family = binomial, data = lf.rel.abun)
summary(mo1lf.liana.time.rel.abu)

#mo1lf.liana.time.rel.abu.nb <- glmer.nb(cbind(liana,nonlianas)~ Treatment*log(Time+1) + (1 |Site/Plot/Month), data = lf.rel.abun)
#summary(mo1lf.liana.time.nb)

mo1lf.liana.time.rel.abu.zi <- glmmTMB(cbind(liana,nonlianas)~ Treatment*log(Time+1) + (1 |Site/Plot/Month), ziformula=~1, family = binomial, data = lf.rel.abun)
summary(mo1lf.liana.time.rel.abu.zi)

#again better fit negative binomial
#the singular fit in both models is because 
#there?s almost no temporal variation (month in the random effects)
lrtest(mo1lf.liana.time.rel.abu, mo1lf.liana.time.rel.abu.zi)

car::Anova(mo1lf.liana.time.rel.abu, type = "III", test.statistic="Chisq") # best model


# no interaction model
mo1lf.liana.rel.abu <- glmer(cbind(liana,nonlianas) ~ (1 |Site/Plot/Month), family=binomial, data = lf.rel.abun)
summary(mo1lf.liana.rel.abu)


mo1lf.liana.rel.abu.zi <- glmmTMB(cbind(liana,nonlianas) ~ (1 |Site/Plot/Month), ziformula=~1, family=binomial, data = lf.rel.abun)
summary(mo1lf.liana.rel.abu.zi)


car::Anova(mo1lf.liana.rel.abu, type = "III", test.statistic="Chisq") # best model


# testing the interaction contribution Weight
anova(mo1lf.liana.time.rel.abu, mo1lf.liana.rel.abu)


# shrub -------------------------------------------------------------------


plotNormalHistogram(lf.rel.abun$shrub)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(lf.rel.abun$shrub+1))


# GLMM


mo1lf.shrub.time.rel.abu <- glmer(cbind(shrub,nonshrubs) ~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family = binomial, data = lf.rel.abun)
summary(mo1lf.shrub.time.rel.abu)


mo1lf.shrub.time.rel.abu.zi <- glmmTMB(cbind(shrub,nonshrubs)~ Treatment*log(Time+1) + (1 |Site/Plot/Month), ziformula=~1, family = binomial, data = lf.rel.abun)
summary(mo1lf.shrub.time.rel.abu.zi)


lrtest(mo1lf.shrub.time.rel.abu, mo1lf.shrub.time.rel.abu.zi)

car::Anova(mo1lf.shrub.time.rel.abu.zi, type = "III", test.statistic="Chisq") # best model



# no interaction model
mo1lf.shrub.rel.abu <- glmer(cbind(shrub,nonshrubs) ~ (1 |Site/Plot/Month), family=binomial, data = lf.rel.abun)
summary(mo1lf.shrub.rel.abu)


mo1lf.shrub.rel.abu.zi <- glmmTMB(cbind(shrub,nonshrubs) ~ (1 |Site/Plot/Month), ziformula=~1, family=binomial, data = lf.rel.abun)
summary(mo1lf.shrub.rel.abu.zi)


car::Anova(mo1lf.shrub.rel.abu.zi, type = "III", test.statistic="Chisq") # best model


# testing the interaction contribution Weight
anova(mo1lf.shrub.time.rel.abu.zi, mo1lf.shrub.rel.abu.zi)



# herbs -------------------------------------------------------------------




plotNormalHistogram(lf.rel.abun$herb)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(lf.rel.abun$herb+1))


# GLMM


mo1lf.herb.time.rel.abu <- glmer(cbind(herb,nonherbs) ~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family = binomial, data = lf.rel.abun)
summary(mo1lf.herb.time.rel.abu)


mo1lf.herb.time.rel.abu.zi <- glmmTMB(cbind(herb,nonherbs)~ Treatment*log(Time+1) + (1 |Site/Plot/Month), ziformula=~1, family = binomial, data = lf.rel.abun)
summary(mo1lf.herb.time.rel.abu.zi)


lrtest(mo1lf.herb.time.rel.abu, mo1lf.herb.time.rel.abu.zi)

car::Anova(mo1lf.herb.time.rel.abu, type = "III", test.statistic="Chisq") # best model


# no interaction model
mo1lf.herb.rel.abu <- glmer(cbind(herb,nonherbs) ~ (1 |Site/Plot/Month), family=binomial, data = lf.rel.abun)
summary(mo1lf.herb.rel.abu)


mo1lf.herb.rel.abu.zi <- glmmTMB(cbind(herb,nonherbs) ~ (1 |Site/Plot/Month), ziformula=~1, family=binomial, data = lf.rel.abun)
summary(mo1lf.herb.rel.abu.zi)


car::Anova(mo1lf.herb.rel.abu, type = "III", test.statistic="Chisq") # best model


# testing the interaction contribution Weight
anova(mo1lf.herb.time.rel.abu, mo1lf.herb.rel.abu)




# bamboos -----------------------------------------------------------------




plotNormalHistogram(lf.rel.abun$bamboo)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(lf.rel.abun$bamboo+1))


# GLMM


mo1lf.bamboo.time.rel.abu <- glmer(cbind(bamboo,nonbamboos) ~ Treatment*log(Time+1) + (1 |Site/Plot/Month), family = binomial, data = lf.rel.abun)
summary(mo1lf.bamboo.time.rel.abu)


mo1lf.bamboo.time.rel.abu.zi <- glmmTMB(cbind(bamboo,nonbamboos)~ Treatment*log(Time+1) + (1 |Site/Plot/Month), ziformula=~1, family = binomial, data = lf.rel.abun)
summary(mo1lf.bamboo.time.rel.abu.zi)


lrtest(mo1lf.bamboo.time.rel.abu, mo1lf.bamboo.time.rel.abu.zi)

car::Anova(mo1lf.bamboo.time.rel.abu.zi, type = "III", test.statistic="Chisq") # best model


# no interaction model
mo1lf.bamboo.rel.abu <- glmer(cbind(bamboo,nonbamboos) ~ (1 |Site/Plot/Month), family=binomial, data = lf.rel.abun)
summary(mo1lf.bamboo.rel.abu)


mo1lf.bamboo.rel.abu.zi <- glmmTMB(cbind(bamboo,nonbamboos) ~ (1 |Site/Plot/Month), ziformula=~1, family=binomial, data = lf.rel.abun)
summary(mo1lf.bamboo.rel.abu.zi) # ANOVA does not work for this


car::Anova(mo1lf.bamboo.rel.abu, type = "III", test.statistic="Chisq") # best model because ANOVA does not worked to ZI model


# testing the interaction contribution Weight
anova(mo1lf.bamboo.time.rel.abu.zi, mo1lf.bamboo.rel.abu.zi) # ANOVA compartion worked to the best model ZI

