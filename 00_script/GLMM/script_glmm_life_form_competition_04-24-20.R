### title: GLMM ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 24/04/2019
### Description: preparacao da tabela de long screen para wide screen e criacao dos modelos de GLMM para formas de vida de plantas


# load packages and table -------------------------------------------------



library(tidyverse)
library(stringr)
library(lme4) 
library(car) # para anova
library(naniar)
library(ggpubr)
library(rcompanion)
library(MASS)
library(lmtest)

### Fitting the data

data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.csv")
glimpse(data_biota)


lf.ttl.abun <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4,24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% # copia a coluna de tempo e transforma em numerico
  group_by(Site, Treatment, Plot, Month, Time, life_form) %>% 
  summarise(Abundance = sum(value)) %>%
  #mutate(Abundance = Abundance / sum(Abundance)) %>% 
  spread(life_form, Abundance) %>%  #converte para widescreen+
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed")))
lf.ttl.abun


# exemplos ----------------------------------------------------------------


### exemplos de como montar um GLMM

#mo1 <- glmer(tree ~ log(Time + 1)*Treatment+ (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#mo2 <- glmer(tree ~ -1 + log(Time + 1)*Treatment + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun) # com o -1 ele já faz a soma dos valores estimados
#mo3 <- glmer(tree ~ -1 + log(Time + 1) + Treatment + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun) # soma o tempo e o tratamento, tempo é positivo


### efeito de outras formas de vida difere entre tratamentos 




# arvores -----------------------------------------------------------------

## arvores x tratamentos

#trees clearly follow a Poisson distribution
plotNormalHistogram(lf.ttl.abun$tree)
#when logged (which is what glmer does) the distribution is almost perfectlly gaussian
plotNormalHistogram(log(lf.ttl.abun$tree+1))

mo1lf.tree.treat <- glmer(tree ~ Treatment + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.tree.treat)

car::Anova(mo1lf.tree.treat, type = "III")

#ggplot(lf.ttl.abun, aes(x = log(Time+1), y = tree, color = Treatment)) + geom_smooth(method = "lm")


## arvores x grupos x tratamentos
# lianas
mo1lf.tree.liana <- glmer(tree ~ Treatment*log(liana+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.tree.liana)

#plot(log(lf.ttl.abun$tree+1)~log(lf.ttl.abun$liana+1))
car::Anova(mo1lf.tree.liana, type = "III")

ggplot(lf.ttl.abun, aes(x = log(liana + 1), y = log(tree + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Trees") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = c(0.7, .19),
        legend.title = element_text(size = 34),
        legend.text = element_text(size = 34)) + 
  scale_color_brewer(palette = "Dark2", labels = c("Open", "Closed"))

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/treesxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")

# palmeiras
mo1lf.tree.palm <- glmer(tree ~ Treatment*log(palm+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.tree.palm)


car::Anova(mo1lf.tree.palm, type = "III")

ggplot(lf.ttl.abun, aes(x = log(palm + 1), y = log(tree + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Trees") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/treesxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")


# arbusto
mo1lf.tree.shrub <- glmer(tree ~ Treatment*log(shrub+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.tree.shrub)

car::Anova(mo1lf.tree.shrub, type = "III")

ggplot(lf.ttl.abun, aes(x = log(shrub + 1), y = log(tree + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Trees") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/treesxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")


# ervas
mo1lf.tree.herbs <- glmer(tree ~ Treatment*log(herb+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.tree.herbs)

car::Anova(mo1lf.tree.herbs, type = "III")

ggplot(lf.ttl.abun, aes(x = log(herb + 1), y = log(tree + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Trees") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/treesxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")

# bamboo
mo1lf.tree.bamboo <- glmer(tree ~ Treatment*log(bamboo+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.tree.bamboo)

car::Anova(mo1lf.tree.bamboo, type = "III")

ggplot(lf.ttl.abun, aes(x = log(bamboo + 1), y = log(tree + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Trees") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2") 

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/treesxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")

# grafico de significancias dos modelos


library(broom)
library(dotwhisker)
library(RColorBrewer)


dwplot(list(mo1lf.tree.liana, mo1lf.tree.palm, mo1lf.tree.shrub, 
            mo1lf.tree.herbs, mo1lf.tree.bamboo), show_intercept = FALSE,
       dodge_size = .5, dot_args = list(size = 2.5), 
       style = "dotwhisker", whisker_args = list(size = 1), 
       effect="fixed") + 
  scale_y_discrete(labels=c("Bamboos-Open", "Bamboos-Closed", 
                            "Shrubs-Open", "Shrubs-Closed",
                            "Herbs-Open", "Herbs-Closed",
                            "Palms-Open", "Palms-Closed",
                            "Lianas-Open", "Lianas-Closed")) +
  geom_vline(xintercept=0, colour = "darkgray", size = 0.7, linetype=2) + 
  scale_colour_manual(values = c("red", "blue","red", "blue","red","red", "blue","red", "blue","red")) + 
  theme_bw() +
  #xlab("Coefficient Estimate") + ylab("") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "none",
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .8)



library(jtools)

plot_summs(mo1lf.tree.liana, mo1lf.tree.palm, mo1lf.tree.shrub, 
           mo1lf.tree.herbs, mo1lf.tree.bamboo, scale = FALSE,
           robust = list(FALSE, "liana", "palm", "herb", "shrub", "bamboo"),
           model.names = c("liana", "palm", "herb", "shrub", "bamboo")) + 
  theme_classic() + scale_y_discrete(labels=c("Bamboos-Open", "Bamboos-Closed", 
                                              "Shrubs-Open", "Shrubs-Closed",
                                              "Herbs-Open", "Herbs-Closed",
                                              "Palms-Open", "Palms-Closed",
                                              "Lianas-Open", "Lianas-Closed"))



# lianas ------------------------------------------------------------------


#mo1lf.liana.treat <- glmer(liana ~ Treatment + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#summary(mo1lf.liana.treat)

mo1lf.liana.treat.nb <- glmer.nb(liana ~ Treatment + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.liana.treat.nb)

#lrtest(mo1lf.liana.treat,mo1lf.liana.treat.nb)

car::Anova(mo1lf.liana.treat.nb, type = "III")

ggplot(lf.ttl.abun, aes(x = Time, y = log(liana + 1), color = Treatment)) + geom_smooth(method = "lm")




# arvores
#mo1lf.liana.tree <- glmer(liana ~  Treatment*log(tree+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#summary(mo1lf.liana.tree)

mo1lf.liana.tree.nb <- glmer.nb(liana ~  Treatment*log(tree+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.liana.tree.nb)
car::Anova(mo1lf.liana.tree.nb, type = "III")

#lrtest(mo1lf.liana.tree,mo1lf.liana.tree.nb)

mo1lf.liana.no.int <- glmer.nb(liana ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.liana.tree.nb, mo1lf.liana.no.int)


ggplot(lf.ttl.abun, aes(x = log(tree+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")




# palmeiras
#mo1lf.liana.palm <- glmer(liana ~  Treatment*log(palm+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#summary(mo1lf.liana.palm)

mo1lf.liana.palm.nb <- glmer.nb(liana ~  Treatment*log(palm+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.liana.palm.nb)
car::Anova(mo1lf.liana.palm.nb, type = "III")

#lrtest(mo1lf.liana.palm,mo1lf.liana.palm.nb)

mo1lf.liana.no.int <- glmer.nb(liana ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.liana.palm.nb, mo1lf.liana.no.int)


ggplot(lf.ttl.abun, aes(x = log(palm+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")




# arbusto
#mo1lf.liana.shrub <- glmer(liana ~  Treatment*log(shrub+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#summary(mo1lf.liana.shrub)

mo1lf.liana.shrub.nb <- glmer.nb(liana ~  Treatment*log(shrub+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.liana.shrub.nb)
car::Anova(mo1lf.liana.shrub.nb, type = "III")

#lrtest(mo1lf.liana.shrub,mo1lf.liana.shrub.nb)

mo1lf.liana.no.int <- glmer.nb(liana ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.liana.shrub.nb, mo1lf.liana.no.int)



ggplot(lf.ttl.abun, aes(x = log(shrub+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")




# ervas
#mo1lf.liana.herb <- glmer(liana ~  Treatment*log(herb+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#summary(mo1lf.liana.herb)

mo1lf.liana.herb.nb <- glmer.nb(liana ~  Treatment*log(herb+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.liana.herb.nb)
car::Anova(mo1lf.liana.herb.nb, type = "III")

#lrtest(mo1lf.liana.herb,mo1lf.liana.herb.nb)

mo1lf.liana.no.int <- glmer.nb(liana ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.liana.herb.nb, mo1lf.liana.no.int)



ggplot(lf.ttl.abun, aes(x = log(herb+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")




# bamboo
#mo1lf.liana.bamboo <- glmer(liana ~  Treatment*log(bamboo+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#summary(mo1lf.liana.bamboo)

mo1lf.liana.bamboo.nb <- glmer.nb(liana ~  Treatment*log(bamboo+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.liana.bamboo.nb)
car::Anova(mo1lf.liana.bamboo.nb, type = "III")

#lrtest(mo1lf.liana.bamboo,mo1lf.liana.bamboo.nb)

mo1lf.liana.no.int <- glmer.nb(liana ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.liana.bamboo.nb, mo1lf.liana.no.int)


ggplot(lf.ttl.abun, aes(x = log(bamboo+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")





# palmeiras ---------------------------------------------------------------



## palmeiras x tratamentos

mo1lf.palm.treat <- glmer(palm ~ Treatment + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.palm.treat)

car::Anova(mo1lf.palm.treat, type = "III")

ggplot(lf.ttl.abun, aes(x = Time, y = log(palm + 1), color = Treatment)) + geom_smooth(method = "lm")


## palmeiras x grupos x tratamentos
# arvores
mo1lf.palm.tree <- glmer(palm ~  Treatment*log(tree+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.palm.tree)

car::Anova(mo1lf.palm.tree, type = "III")

mo1lf.palm.no.int <- glmer(palm ~ (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
anova(mo1lf.palm.tree, mo1lf.palm.no.int)



ggplot(lf.ttl.abun, aes(x = log(tree + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")




# lianas
mo1lf.palm.liana <- glmer(palm ~  Treatment*log(liana+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.palm.liana)

car::Anova(mo1lf.palm.liana, type = "III")

mo1lf.palm.no.int <- glmer(palm ~ (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
anova(mo1lf.palm.liana, mo1lf.palm.no.int)



ggplot(lf.ttl.abun, aes(x = log(liana + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")




# arbusto
mo1lf.palm.shrub <- glmer(palm ~  Treatment*log(shrub+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.palm.shrub)

car::Anova(mo1lf.palm.shrub, type = "III")

mo1lf.palm.no.int <- glmer(palm ~ (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
anova(mo1lf.palm.shrub, mo1lf.palm.no.int)


ggplot(lf.ttl.abun, aes(x = log(shrub + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")




# ervas
mo1lf.palm.herb <- glmer(palm ~  Treatment*log(herb+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.palm.herb)

car::Anova(mo1lf.palm.herb, type = "III")

mo1lf.palm.no.int <- glmer(palm ~ (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
anova(mo1lf.palm.herb, mo1lf.palm.no.int)


ggplot(lf.ttl.abun, aes(x = log(herb + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")




# bamboo
mo1lf.palm.bamboo <- glmer(palm ~  Treatment*log(bamboo+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.palm.bamboo)

car::Anova(mo1lf.palm.bamboo, type = "III")

mo1lf.palm.no.int <- glmer(palm ~ (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
anova(mo1lf.palm.bamboo, mo1lf.palm.no.int)


ggplot(lf.ttl.abun, aes(x = log(bamboo + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2") + expand_limits(y=c(1,2.6))

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")


# grafico de significancias dos modelos

#dwplot(list(mo1lf.palm.treat, mo1lf.palm.tree, mo1lf.palm.liana, mo1lf.palm.shrub, mo1lf.palm.herbs, mo1lf.palm.bamboo),effect="fixed")+
#  geom_vline(xintercept=0,lty=2) + theme_bw()






# arbustos ----------------------------------------------------------------


## arbustos x tratamentos

mo1lf.shrub.treat.nb <- glmer.nb(shrub ~ Treatment + (1 |Site/Plot/Month),  data = lf.ttl.abun)
summary(mo1lf.shrub.treat.nb)

car::Anova(mo1lf.shrub.treat.nb, type = "III")

ggplot(lf.ttl.abun, aes(x = Time, y = log(shrub + 1), color = Treatment)) + geom_smooth(method = "lm")


## arbustos x grupos x tratamentos

# arvores
#mo1lf.shrub.tree <- glmer(shrub ~  Treatment*log(tree+1) + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
#summary(mo1lf.shrub.tree)

mo1lf.shrub.tree.nb <- glmer.nb(shrub ~  Treatment*log(tree+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.shrub.tree.nb)
car::Anova(mo1lf.shrub.tree.nb, type = "III")

#lrtest(mo1lf.shrub.tree,mo1lf.shrub.tree.nb)

mo1lf.shrub.no.int <- glmer.nb(shrub ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.shrub.tree.nb, mo1lf.shrub.no.int)



ggplot(lf.ttl.abun, aes(x = log(tree + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")




# lianas

mo1lf.shrub.liana.nb <- glmer.nb(shrub ~  Treatment*log(liana+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.shrub.liana.nb)
car::Anova(mo1lf.shrub.liana.nb, type = "III")

#lrtest(mo1lf.shrub.liana,mo1lf.shrub.liana.nb)

mo1lf.shrub.no.int <- glmer.nb(shrub ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.shrub.liana.nb, mo1lf.shrub.no.int)


ggplot(lf.ttl.abun, aes(x = log(liana + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")




# palmeiras

mo1lf.shrub.palm.nb <- glmer.nb(shrub ~  Treatment*log(palm+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.shrub.palm.nb)
car::Anova(mo1lf.shrub.palm.nb, type = "III")

#lrtest(mo1lf.shrub.palm,mo1lf.shrub.palm.nb)

mo1lf.shrub.no.int <- glmer.nb(shrub ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.shrub.palm.nb, mo1lf.shrub.no.int)


ggplot(lf.ttl.abun, aes(x = log(palm + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")





# ervas

mo1lf.shrub.herb.nb <- glmer.nb(shrub ~  Treatment*log(herb+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.shrub.herb.nb)
car::Anova(mo1lf.shrub.herb.nb, type = "III")

#lrtest(mo1lf.shrub.herb,mo1lf.shrub.herb.nb)

mo1lf.shrub.no.int <- glmer.nb(shrub ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.shrub.herb.nb, mo1lf.shrub.no.int)


ggplot(lf.ttl.abun, aes(x = log(herb + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")





# bamboo

mo1lf.shrub.bamboo.nb <- glmer.nb(shrub ~  Treatment*log(bamboo+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.shrub.bamboo.nb)
car::Anova(mo1lf.shrub.bamboo.nb, type = "III")

#lrtest(mo1lf.shrub.bamboo,mo1lf.shrub.bamboo.nb)

mo1lf.shrub.no.int <- glmer.nb(shrub ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.shrub.bamboo.nb, mo1lf.shrub.no.int)


ggplot(lf.ttl.abun, aes(x = log(bamboo + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")



# grafico de significancias dos modelos

#dwplot(list(mo1lf.shrub.treat, mo1lf.shrub.tree, mo1lf.shrub.liana, mo1lf.shrub.palm, mo1lf.shrub.herbs, mo1lf.shrub.bamboo),effect="fixed")+
#  geom_vline(xintercept=0,lty=2) + theme_bw()






# ervas -------------------------------------------------------------------

## ervas x tratamentos

mo1lf.herb.treat.nb <- glmer(herb ~ Treatment + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.herb.treat.nb)

car::Anova(mo1lf.herb.treat.nb, type = "III")

ggplot(lf.ttl.abun, aes(Time, y = log(herb + 1), color = Treatment)) + geom_smooth(method = "lm")




## ervas x grupos x tratamentos

# arvores

mo1lf.herb.tree.nb <- glmer.nb(herb ~  Treatment*log(tree+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.herb.tree.nb)
car::Anova(mo1lf.herb.tree.nb, type = "III")

#lrtest(mo1lf.herb.tree,mo1lf.herb.tree.nb)

mo1lf.herb.no.int <- glmer.nb(herb ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.herb.tree.nb, mo1lf.herb.no.int)


ggplot(lf.ttl.abun, aes(x = log(tree + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")




# lianas

mo1lf.herb.liana.nb <- glmer.nb(herb ~  Treatment*log(liana+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.herb.liana.nb)
car::Anova(mo1lf.herb.liana.nb, type = "III")

#lrtest(mo1lf.herb.liana,mo1lf.herb.liana.nb)

mo1lf.herb.no.int <- glmer.nb(herb ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.herb.liana.nb, mo1lf.herb.no.int)


ggplot(lf.ttl.abun, aes(x = log(liana + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")




# palmeiras

mo1lf.herb.palm.nb <- glmer.nb(herb ~  Treatment*log(palm+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.herb.palm.nb)
car::Anova(mo1lf.herb.palm.nb, type = "III")

#lrtest(mo1lf.herb.palm,mo1lf.herb.palm.nb)

mo1lf.herb.no.int <- glmer.nb(herb ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.herb.palm.nb, mo1lf.herb.no.int)


ggplot(lf.ttl.abun, aes(x = log(palm + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")




# arbustos

mo1lf.herb.shrub.nb <- glmer.nb(herb ~  Treatment*log(shrub+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.herb.shrub.nb)
car::Anova(mo1lf.herb.shrub.nb, type = "III")

#lrtest(mo1lf.herb.shrub,mo1lf.herb.shrub.nb)

mo1lf.herb.no.int <- glmer.nb(herb ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.herb.shrub.nb, mo1lf.herb.no.int)


ggplot(lf.ttl.abun, aes(x = log(shrub + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2") + expand_limits(y=c(1,2.25))

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")




# bamboo

mo1lf.herb.bamboo.nb <- glmer.nb(herb ~  Treatment*log(bamboo+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.herb.bamboo.nb)
car::Anova(mo1lf.herb.bamboo.nb, type = "III")

#lrtest(mo1lf.herb.bamboo,mo1lf.herb.bamboo.nb)

mo1lf.herb.no.int <- glmer.nb(herb ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.herb.bamboo.nb, mo1lf.herb.no.int)


ggplot(lf.ttl.abun, aes(x = log(bamboo + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")


# grafico de significancias dos modelos

#dwplot(list(mo1lf.herb.treat, mo1lf.herb.tree.sc, mo1lf.herb.liana.sc, mo1lf.herb.palm, mo1lf.herb.shrub, mo1lf.herb.bamboo),effect="fixed")+
#  geom_vline(xintercept=0,lty=2) + theme_bw()








# bamboos -------------------------------------------------------------------------


## bamboos x tratamentos

mo1lf.bamboo.treat.nb <- glmer(bamboo ~ Treatment + (1 |Site/Plot/Month), family = poisson, data = lf.ttl.abun)
summary(mo1lf.bamboo.treat.nb)

car::Anova(mo1lf.bamboo.treat.nb, type = "III")

ggplot(lf.ttl.abun, aes(x = Time, y = log(bamboo + 1), color = Treatment)) + geom_smooth(method = "lm")




## bamboos x grupos x tratamentos

# arvores

mo1lf.bamboo.tree.nb <- glmer.nb(bamboo ~  Treatment*log(tree+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.bamboo.tree.nb)
car::Anova(mo1lf.bamboo.tree.nb, type = "III")

#lrtest(mo1lf.bamboo.tree,mo1lf.bamboo.tree.nb)

mo1lf.bamboo.no.int <- glmer.nb(bamboo ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.bamboo.tree.nb, mo1lf.bamboo.no.int)



ggplot(lf.ttl.abun, aes(x = log(tree + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")




# lianas
mo1lf.bamboo.liana.nb <- glmer.nb(bamboo ~  Treatment*log(liana+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.bamboo.liana.nb)
car::Anova(mo1lf.bamboo.liana.nb, type = "III")

#lrtest(mo1lf.bamboo.liana,mo1lf.bamboo.liana.nb)

mo1lf.bamboo.no.int <- glmer.nb(bamboo ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.bamboo.liana.nb, mo1lf.bamboo.no.int)


ggplot(lf.ttl.abun, aes(x = log(liana + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")




# palmeiras
mo1lf.bamboo.palm.nb <- glmer.nb(bamboo ~  Treatment*log(palm+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.bamboo.palm.nb)
car::Anova(mo1lf.bamboo.palm.nb, type = "III")

#lrtest(mo1lf.bamboo.palm,mo1lf.bamboo.palm.nb)

mo1lf.bamboo.no.int <- glmer.nb(bamboo ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.bamboo.palm.nb, mo1lf.bamboo.no.int)


ggplot(lf.ttl.abun, aes(x = log(palm + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")




# arbustos
mo1lf.bamboo.shrub.nb <- glmer.nb(bamboo ~  Treatment*log(shrub+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.bamboo.shrub.nb)
car::Anova(mo1lf.bamboo.shrub.nb, type = "III")

#lrtest(mo1lf.bamboo.shrub,mo1lf.bamboo.shrub.nb)

mo1lf.bamboo.no.int <- glmer.nb(bamboo ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.bamboo.shrub.nb, mo1lf.bamboo.no.int)



ggplot(lf.ttl.abun, aes(x = log(shrub + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")




# herbs
mo1lf.bamboo.herb.nb <- glmer.nb(bamboo ~  Treatment*log(herb+1) + (1 |Site/Plot/Month), data = lf.ttl.abun)
summary(mo1lf.bamboo.herb.nb)
car::Anova(mo1lf.bamboo.herb.nb, type = "III")

#lrtest(mo1lf.bamboo.herb,mo1lf.bamboo.herb.nb)

mo1lf.bamboo.no.int <- glmer.nb(bamboo ~ (1 |Site/Plot/Month), data = lf.ttl.abun)
anova(mo1lf.bamboo.herb.nb, mo1lf.bamboo.no.int)


ggplot(lf.ttl.abun, aes(x = log(herb + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

#ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")


# grafico de significancias dos modelos

#dwplot(list(mo1lf.bamboo.treat, mo1lf.bamboo.tree, mo1lf.bamboo.liana, mo1lf.bamboo.palm, mo1lf.bamboo.shrub, mo1lf.bamboo.herb),effect="fixed")+
#  geom_vline(xintercept=0,lty=2) + theme_bw()



# outros graficos-------------------------------------------------------------------------



#trees_models <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/glmm/trees_glmm_summary.csv")
#trees_models <- as_tibble(trees_models)
#trees_models$term <- as.factor(trees_models$term)
#trees_models
#?geom_vline

#?dwplot
#dwplot(trees_models, dodge_size = .70, size = 5, 
#       by_2sd = TRUE, 
#       dot_args = list(size = 4, shape = 21, stroke = 1.7, fill = "grey80")) + 
#  geom_vline(xintercept = 0, colour = "grey40", linetype = 5) + # plot line at zero _behind_ coefs
#  theme_bw() +
#  xlab("Coefficient Estimate") + ylab("Trees competitors") +
#  theme(plot.title = element_text(face="bold"),
#        legend.position = c(0.007, 0.65),
#        legend.justification = c(0, 0),
#        legend.background = element_rect(colour="grey80"),
#        legend.title.align = .5)

#ggsave("C:/Users/Yuri/Desktop/aaa.png", width = 30, height = 10, units = "cm", dpi = 300)


