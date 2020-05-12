### title: GLMM - Density ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 10/03/2019
### Description: preparacao da tabela de long screen para wide screen e criacao dos modelos de GLMM para formas de vida de plantas


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
  mutate(bamboo_d = bamboo/(3^2),
         herb_d = herb/(3^2),
         liana_d = liana/(3^2),
         palm_d = palm/(3^2),
         shrub_d = shrub/(3^2),
         tree_d = tree/(3^2))
total.lf


## trees
#shapiro.test(total.lf$tree)
#hist(total.lf$tree)
#ggdensity(total.lf$tree)
#ggqqplot(total.lf$tree)

mo1lf.tree.time <- glmer(tree ~ Treatment*log(time+1) + (1 |Site/month ), family=poisson, data = total.lf)
summary(mo1lf.tree.time)
car::Anova(mo1lf.tree.time, type = "III")
anova(mo1lf.tree.time)

mo1lf.tree <- glmer(tree ~ (1 |Site/month ), family=poisson, data = total.lf)
summary(mo1lf.tree)
car::Anova(mo1lf.tree, type = "III")
anova(mo1lf.tree)

anova(mo1lf.tree.time,mo1lf.tree)


## palm

mo1lf.palm.time <- glmer(palm ~ Treatment*log(time+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.palm.time)
car::Anova(mo1lf.palm.time, type = "III")
anova(mo1lf.palm.time)

mo1lf.palm <- glmer(palm ~ (1 |Site/month ), family=poisson, data = total.lf)
summary(mo1lf.palm)
car::Anova(mo1lf.palm, type = "III")
anova(mo1lf.palm)

anova(mo1lf.palm.time,mo1lf.palm)



## lianas

mo1lf.liana.time <- glmer(liana ~ Treatment*log(time+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.liana.time)
car::Anova(mo1lf.liana.time, type = "III")
anova(mo1lf.liana.time)


mo1lf.liana <- glmer(liana ~ (1 |Site/month ), family=poisson, data = total.lf)
summary(mo1lf.liana)
car::Anova(mo1lf.liana, type = "III")
anova(mo1lf.liana)

anova(mo1lf.liana.time,mo1lf.liana)


## shrub

mo1lf.shrub.time <- glmer(shrub ~ Treatment*log(time+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.shrub.time)
car::Anova(mo1lf.shrub.time, type = "III")
anova(mo1lf.shrub.time)


mo1lf.shrub <- glmer(shrub ~ (1 |Site/month ), family=poisson, data = total.lf)
summary(mo1lf.shrub)
car::Anova(mo1lf.shrub, type = "III")
anova(mo1lf.shrub)

anova(mo1lf.shrub.time,mo1lf.shrub)

## herbs

mo1lf.herb.time <- glmer(herb ~ Treatment*log(time+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.herb.time)
car::Anova(mo1lf.herb.time, type = "III")
anova(mo1lf.herb.time)

mo1lf.herb <- glmer(herb ~ (1 |Site/month ), family=poisson, data = total.lf)
summary(mo1lf.herb)
car::Anova(mo1lf.herb, type = "III")
anova(mo1lf.herb)

anova(mo1lf.herb.time,mo1lf.herb)


## bamboos

mo1lf.bamboo.time <- glmer(bamboo ~ Treatment*log(time+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.bamboo.time)
car::Anova(mo1lf.bamboo.time, type = "III")
anova(mo1lf.bamboo.time)


mo1lf.herb <- glmer(herb ~ (1 |Site/month ), family=poisson, data = total.lf)
summary(mo1lf.herb)
car::Anova(mo1lf.herb, type = "III")
anova(mo1lf.herb)

anova(mo1lf.herb.time,mo1lf.herb)
