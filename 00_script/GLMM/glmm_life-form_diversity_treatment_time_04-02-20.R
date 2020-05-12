### title: GLMM - Diversity ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 02/04/2020
### Description: create the models to test significance of life-form evenness through time using GLMM test


rm(list = ls()) 


# packages ----------------------------------------------------------------



library(tidyverse)
library(stringr)
library(lme4) 
library(car) # para anova
library(naniar)
library(ggpubr)


# table -------------------------------------------------------------------


bt.div.evenn <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/diversity/total.lf.diversity_31-03-20.csv")

# trees -------------------------------------------------------------------


#shapiro.test(total.lf$tree)
#hist(total.lf$tree)
#ggdensity(total.lf$tree)
#ggqqplot(total.lf$tree)

bt.div.evenn.tree <- bt.div.evenn %>% 
  filter(life_form == "trees") %>% 
  mutate(SimpsonEvenness_trees = SimpsonEvenness+0.00000001) %>% 
  mutate(month = as.factor(time)) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_relevel(treatment, c("open", "closed"))) %>% 
  select(c(1:4,12,5:7,9,11,10))
bt.div.evenn.tree 

bt.div.evenn.tree.glmm <- glmer(SimpsonEvenness_trees ~ treatment*log(time+1) + (1 |site/month ), family=Gamma, data = bt.div.evenn.tree)
summary(bt.div.evenn.tree.glmm)
car::Anova(bt.div.evenn.tree.glmm, type = "III")
anova(bt.div.evenn.tree.glmm)



# palms -------------------------------------------------------------------

bt.div.evenn.palm <- bt.div.evenn %>% 
  filter(life_form == "palms") %>% 
  mutate(SimpsonEvenness_palms = SimpsonEvenness+0.00000001) %>% 
  mutate(month = as.factor(time)) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_relevel(treatment, c("open", "closed"))) %>% 
  select(c(1:4,12,5:7,9,11,10))
bt.div.evenn.palm 

bt.div.evenn.palm.glmm <- glmer(SimpsonEvenness_palms ~ treatment*log(time+1) + (1 |site/month ), family=Gamma, data = bt.div.evenn.palm)
summary(bt.div.evenn.palm.glmm)
car::Anova(bt.div.evenn.palm.glmm, type = "III")
anova(bt.div.evenn.palm.glmm)


# lianas ------------------------------------------------------------------

bt.div.evenn.liana <- bt.div.evenn %>% 
  filter(life_form == "lianas") %>% 
  mutate(SimpsonEvenness_lianas = SimpsonEvenness+0.00000001) %>% 
  mutate(month = as.factor(time)) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_relevel(treatment, c("open", "closed"))) %>% 
  select(c(1:4,12,5:7,9,11,10))
bt.div.evenn.liana 

bt.div.evenn.liana.glmm <- glmer(SimpsonEvenness_lianas ~ treatment*log(time+1) + (1 |site/month ), family=Gamma, data = bt.div.evenn.liana)
summary(bt.div.evenn.liana.glmm)
car::Anova(bt.div.evenn.liana.glmm, type = "III")
anova(bt.div.evenn.liana.glmm)


# shrubs -------------------------------------------------------------------

bt.div.evenn.shrub <- bt.div.evenn %>% 
  filter(life_form == "shrubs") %>% 
  mutate(SimpsonEvenness_shrubs = SimpsonEvenness+0.00000001) %>% 
  mutate(month = as.factor(time)) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_relevel(treatment, c("open", "closed"))) %>% 
  select(c(1:4,12,5:7,9,11,10))
bt.div.evenn.shrub 

bt.div.evenn.shrub.glmm <- glmer(SimpsonEvenness_shrubs ~ treatment*log(time+1) + (1 |site/month ), family=Gamma, data = bt.div.evenn.shrub)
summary(bt.div.evenn.shrub.glmm)
car::Anova(bt.div.evenn.shrub.glmm, type = "III")
anova(bt.div.evenn.shrub.glmm)


# herbs --------------------------------------------------------------------

bt.div.evenn.herb <- bt.div.evenn %>% 
  filter(life_form == "herbs") %>% 
  mutate(SimpsonEvenness_herbs = SimpsonEvenness+0.00000001) %>% 
  mutate(month = as.factor(time)) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_relevel(treatment, c("open", "closed"))) %>% 
  select(c(1:4,12,5:7,9,11,10))
bt.div.evenn.herb 

bt.div.evenn.herb.glmm <- glmer(SimpsonEvenness_herbs ~ treatment*log(time+1) + (1 |site/month ), family=Gamma, data = bt.div.evenn.herb)
summary(bt.div.evenn.herb.glmm)
car::Anova(bt.div.evenn.herb.glmm, type = "III")
anova(bt.div.evenn.herb.glmm)



# bamboos ------------------------------------------------------------------


bt.div.evenn.bamboo <- bt.div.evenn %>% 
  filter(life_form == "bamboos") %>% 
  mutate(SimpsonEvenness_bamboos = SimpsonEvenness+0.00000001) %>% 
  mutate(month = as.factor(time)) %>%
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment = fct_relevel(treatment, c("open", "closed"))) %>% 
  select(c(1:4,12,5:7,9,11,10))
bt.div.evenn.bamboo 

bt.div.evenn.bamboo.glmm <- glmer(SimpsonEvenness_bamboos ~ treatment*log(time+1) + (1 |site/month ), family=Gamma, data = bt.div.evenn.bamboo)
summary(bt.div.evenn.bamboo.glmm)
car::Anova(bt.div.evenn.bamboo.glmm, type = "III")
anova(bt.div.evenn.bamboo.glmm)

