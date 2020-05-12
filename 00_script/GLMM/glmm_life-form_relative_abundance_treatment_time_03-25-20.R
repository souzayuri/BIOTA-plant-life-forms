### title: GLMM - Relative abundance ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 25/03/2020
### Description: Create a table to test the life-form realtive abundance using GLMM


rm(list = ls()) 

library(tidyverse)
library(stringr)
library(lme4) 
library(car) # para anova
library(naniar)
library(ggpubr)


### Fitting the data

btt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
btt 

total.lf <- btt %>% 
  select(-c(1,5,23:28)) %>% 
  gather(key = "time", value = "value", 4:20) %>% 
  rename(life_form = `Life Form`, month = time) %>% # renomeia a coluna `Life Form`
  filter(life_form != "indeterminate") %>% 
  mutate(time = as.numeric(month %>% stringr::str_replace("p", ""))) %>% # copia a coluna de tempo e transforma em numerico
  group_by(Site, Plot, Treatment, time, month, life_form) %>% 
  summarise(abundancia = sum(value)) %>%
  spread(life_form, abundancia) %>%  #converte para widescreen+
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
total.lf



## trees

mo1lf.tree.time <- glmer(cbind(tree,nontrees) ~ Treatment*log(time+1) + (1 |Site/month ), family=binomial, data = total.lf)
summary(mo1lf.tree.time)
car::Anova(mo1lf.tree.time, type = "III", test.statistic="Chisq")

mo1lf.tree <- glmer(cbind(tree,nontrees) ~ (1 |Site/month ), family=binomial, data = total.lf)
summary(mo1lf.tree)
car::Anova(mo1lf.tree, type = "III", test.statistic="Chisq")

anova(mo1lf.tree.time,mo1lf.tree)

## palm

mo1lf.palm.time <- glmer(cbind(palm,nonpalms) ~ Treatment*log(time+1) + (1 |Site/month ), family = binomial, data = total.lf)
summary(mo1lf.palm.time)
car::Anova(mo1lf.palm.time, type = "III", test.statistic="Chisq")

mo1lf.palm <- glmer(cbind(palm,nonpalms) ~ (1 |Site/month ), family=binomial, data = total.lf)
summary(mo1lf.palm)
car::Anova(mo1lf.palm, type = "III", test.statistic="Chisq")

anova(mo1lf.palm.time,mo1lf.palm)


## lianas

mo1lf.liana.time <- glmer(cbind(liana,nonlianas) ~ Treatment*log(time+1) + (1 |Site/month ), family = binomial, data = total.lf)
summary(mo1lf.liana.time)
car::Anova(mo1lf.liana.time, type = "III", test.statistic="Chisq")

mo1lf.tree <- glmer(cbind(tree,nontrees) ~ (1 |Site/month ), family=binomial, data = total.lf)
summary(mo1lf.tree)
car::Anova(mo1lf.tree, type = "III", test.statistic="Chisq")

anova(mo1lf.tree.time,mo1lf.tree)


## shrub

mo1lf.shrub.time <- glmer(cbind(shrub,nonshrubs) ~ Treatment*log(time+1) + (1 |Site/month ), family = binomial, data = total.lf)
summary(mo1lf.shrub.time)
car::Anova(mo1lf.shrub.time, type = "III", test.statistic="Chisq")

mo1lf.shrub <- glmer(cbind(shrub,nonshrubs) ~ (1 |Site/month ), family=binomial, data = total.lf)
summary(mo1lf.shrub)
car::Anova(mo1lf.shrub, type = "III", test.statistic="Chisq")

anova(mo1lf.shrub.time,mo1lf.shrub)


## herbs

mo1lf.herb.time <- glmer(cbind(herb,nonherbs) ~ Treatment*log(time+1) + (1 |Site/month ), family = binomial, data = total.lf)
summary(mo1lf.herb.time)
car::Anova(mo1lf.herb.time, type = "III", test.statistic="Chisq")

mo1lf.herb <- glmer(cbind(herb,nonherbs) ~ (1 |Site/month ), family=binomial, data = total.lf)
summary(mo1lf.herb)
car::Anova(mo1lf.herb, type = "III", test.statistic="Chisq")

anova(mo1lf.herb.time,mo1lf.herb)


## bamboos

mo1lf.bamboo.time <- glmer(cbind(bamboo,nonbamboos) ~ Treatment*log(time+1) + (1 |Site/month ), family = binomial, data = total.lf)
summary(mo1lf.bamboo.time)
car::Anova(mo1lf.bamboo.time, type = "III", test.statistic="Chisq")

mo1lf.bamboo <- glmer(cbind(bamboo,nonbamboos) ~ (1 |Site/month ), family=binomial, data = total.lf)
summary(mo1lf.bamboo)
car::Anova(mo1lf.bamboo, type = "III", test.statistic="Chisq")

anova(mo1lf.bamboo.time,mo1lf.bamboo)
