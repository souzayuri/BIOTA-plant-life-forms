### title: GLMM ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 10/03/2019
### Description: preparacao da tabela de long screen para wide screen e criacao dos modelos de GLMM para formas de vida de plantas



library(tidyverse)
library(stringr)
library(lme4) 
library(car) # para anova
library(naniar)
library(ggpubr)


### Fitting the data

bt.dt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
bt.dt 

total.lf <- bt.dt %>% 
  select(-c(1,5,23:28)) %>% 
  gather(key = "time", value = "value", 4:20) %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = sum(value)) %>%
  rename(life_form = `Life Form`, month = time) %>% # renomeia a coluna `Life Form`
  mutate(time = as.numeric(month %>% stringr::str_replace("p", ""))) %>% # copia a coluna de tempo e transforma em numerico
  spread(life_form, abundancia) %>%  #converte para widescreen+
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, c("open", "closed")))
total.lf


# exemplos ----------------------------------------------------------------


### exemplos de como montar um GLMM

#mo1 <- glmer(tree ~ log(time + 1)*Treatment+ (1 |Site/month ), family = poisson, data = total.lf)
#mo2 <- glmer(tree ~ -1 + log(time + 1)*Treatment + (1 |Site/month ), family = poisson, data = total.lf) # com o -1 ele já faz a soma dos valores estimados
#mo3 <- glmer(tree ~ -1 + log(time + 1) + Treatment + (1 |Site/month ), family = poisson, data = total.lf) # soma o tempo e o tratamento, tempo é positivo


### efeito de outras formas de vida difere entre tratamentos 




# arvores -----------------------------------------------------------------

## arvores x tratamentos

mo1lf.tree.treat <- glmer(tree ~ Treatment + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.tree.treat)

car::Anova(mo1lf.tree.treat, type = "III")

ggplot(total.lf, aes(x = log(time+1), y = tree, color = Treatment)) + geom_smooth(method = "lm")


## arvores x grupos x tratamentos
# lianas
mo1lf.tree.liana <- glmer(tree ~ Treatment*log(liana+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.tree.liana)

#plot(log(total.lf$tree+1)~log(total.lf$liana+1))
car::Anova(mo1lf.tree.liana, type = "III")

ggplot(total.lf, aes(x = log(liana + 1), y = log(tree + 1), color = Treatment)) + 
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
mo1lf.tree.palm <- glmer(tree ~ Treatment*log(palm+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.tree.palm)


car::Anova(mo1lf.tree.palm, type = "III")

ggplot(total.lf, aes(x = log(palm + 1), y = log(tree + 1), color = Treatment)) + 
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
mo1lf.tree.shrub <- glmer(tree ~ Treatment*log(shrub+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.tree.shrub)

car::Anova(mo1lf.tree.shrub, type = "III")

ggplot(total.lf, aes(x = log(shrub + 1), y = log(tree + 1), color = Treatment)) + 
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
mo1lf.tree.herbs <- glmer(tree ~ Treatment*log(herb+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.tree.herbs)

car::Anova(mo1lf.tree.herbs, type = "III")

ggplot(total.lf, aes(x = log(herb + 1), y = log(tree + 1), color = Treatment)) + 
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
mo1lf.tree.bamboo <- glmer(tree ~ Treatment*log(bamboo+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.tree.bamboo)

car::Anova(mo1lf.tree.bamboo, type = "III")

ggplot(total.lf, aes(x = log(bamboo + 1), y = log(tree + 1), color = Treatment)) + 
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


mo1lf.liana.treat <- glmer(liana ~ Treatment + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.liana.treat)

car::Anova(mo1lf.liana.treat, type = "III")

ggplot(total.lf, aes(x = time, y = log(liana + 1), color = Treatment)) + geom_smooth(method = "lm")


# arvores
mo1lf.liana.tree <- glmer(liana ~  Treatment*log(tree+1) + (1 |Site/month), family = poisson, data = total.lf)
summary(mo1lf.liana.tree)

car::Anova(mo1lf.liana.tree, type = "III")

ggplot(total.lf, aes(x = log(tree+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")


# palmeiras
mo1lf.liana.palm <- glmer(liana ~ Treatment*log(palm+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.liana.palm)

car::Anova(mo1lf.liana.palm, type = "III")

ggplot(total.lf, aes(x = log(palm+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")


# arbusto
mo1lf.liana.shrub <- glmer(liana ~ Treatment*log(shrub+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.liana.shrub)

car::Anova(mo1lf.liana.shrub, type = "III")

ggplot(total.lf, aes(x = log(shrub+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")


# ervas
mo1lf.liana.herbs <- glmer(liana ~ Treatment*log(herb+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.liana.herbs)

car::Anova(mo1lf.liana.herbs, type = "III")

ggplot(total.lf, aes(x = log(herb+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")


# bamboo
mo1lf.liana.bamboo <- glmer(liana ~ Treatment*log(bamboo+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.liana.bamboo)

car::Anova(mo1lf.liana.bamboo, type = "III")

ggplot(total.lf, aes(x = log(bamboo+ 1), y = log(liana + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Lianas") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/lianasxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")





# palmeiras ---------------------------------------------------------------



## palmeiras x tratamentos

mo1lf.palm.treat <- glmer(palm ~ Treatment + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.palm.treat)

car::Anova(mo1lf.palm.treat, type = "III")

ggplot(total.lf, aes(x = time, y = log(palm + 1), color = Treatment)) + geom_smooth(method = "lm")


## palmeiras x grupos x tratamentos
# arvores
mo1lf.palm.tree <- glmer(palm ~  Treatment*log(tree+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.palm.tree)

car::Anova(mo1lf.palm.tree, type = "III")

ggplot(total.lf, aes(x = log(tree + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")


# lianas
mo1lf.palm.liana <- glmer(palm ~ Treatment*log(liana+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.palm.liana)

car::Anova(mo1lf.palm.liana, type = "III")

ggplot(total.lf, aes(x = log(liana + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")


# arbusto
mo1lf.palm.shrub <- glmer(palm ~ Treatment*log(shrub+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.palm.shrub)


car::Anova(mo1lf.palm.shrub, type = "III")

ggplot(total.lf, aes(x = log(shrub + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")


# ervas
mo1lf.palm.herbs <- glmer(palm ~ Treatment*log(herb+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.palm.herbs)


car::Anova(mo1lf.palm.herbs, type = "III")

ggplot(total.lf, aes(x = log(herb + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")


# bamboo
mo1lf.palm.bamboo <- glmer(palm ~ Treatment*log(bamboo+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.palm.bamboo)


car::Anova(mo1lf.palm.bamboo, type = "III")

ggplot(total.lf, aes(x = log(bamboo + 1), y = log(palm + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Palms") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2") + expand_limits(y=c(1,2.6))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/palmsxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")


# grafico de significancias dos modelos

dwplot(list(mo1lf.palm.treat, mo1lf.palm.tree, mo1lf.palm.liana, mo1lf.palm.shrub, mo1lf.palm.herbs, mo1lf.palm.bamboo),effect="fixed")+
  geom_vline(xintercept=0,lty=2) + theme_bw()






# arbustos ----------------------------------------------------------------


## arbustos x tratamentos

mo1lf.shrub.treat <- glmer(shrub ~ Treatment + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.shrub.treat)

car::Anova(mo1lf.shrub.treat, type = "III")

ggplot(total.lf, aes(x = time, y = log(shrub + 1), color = Treatment)) + geom_smooth(method = "lm")


## arbustos x grupos x tratamentos
# arvores
mo1lf.shrub.tree <- glmer(shrub ~ Treatment*log(tree+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.shrub.tree)

car::Anova(mo1lf.shrub.tree, type = "III")


ggplot(total.lf, aes(x = log(tree + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")


# lianas
mo1lf.shrub.liana <- glmer(shrub ~ Treatment*log(liana+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.shrub.liana)

car::Anova(mo1lf.shrub.liana, type = "III")

ggplot(total.lf, aes(x = log(liana + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")


# palmeiras
mo1lf.shrub.palm <- glmer(shrub ~ Treatment*log(palm+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.shrub.palm)

car::Anova(mo1lf.shrub.palm, type = "III")

ggplot(total.lf, aes(x = log(palm + 1), y = log(shrub + 1), color = Treatment)) + 
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
mo1lf.shrub.herbs <- glmer(shrub ~ Treatment*log(herb+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.shrub.herbs)

car::Anova(mo1lf.shrub.herbs, type = "III")

ggplot(total.lf, aes(x = log(herb + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")



# bamboo
mo1lf.shrub.bamboo <- glmer(shrub ~ Treatment*log(bamboo+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.shrub.bamboo)

car::Anova(mo1lf.shrub.bamboo, type = "III")

ggplot(total.lf, aes(x = log(bamboo + 1), y = log(shrub + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Shrubs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/shrubsxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")



# grafico de significancias dos modelos

dwplot(list(mo1lf.shrub.treat, mo1lf.shrub.tree, mo1lf.shrub.liana, mo1lf.shrub.palm, mo1lf.shrub.herbs, mo1lf.shrub.bamboo),effect="fixed")+
  geom_vline(xintercept=0,lty=2) + theme_bw()






# ervas -------------------------------------------------------------------

## ervas x tratamentos

mo1lf.herb.treat <- glmer(herb ~ Treatment + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.herb.treat)

car::Anova(mo1lf.herb.treat, type = "III")

ggplot(total.lf, aes(time, y = log(herb + 1), color = Treatment)) + geom_smooth(method = "lm")


## ervas x grupos x tratamentos

# arvores
mo1lf.herb.tree <- glmer(herb ~ Treatment*log(tree+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.herb.tree)

car::Anova(mo1lf.herb.tree, type = "III")

ggplot(total.lf, aes(x = log(tree + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")


# lianas
mo1lf.herb.liana <- glmer(herb ~ Treatment*log(liana+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.herb.liana)

car::Anova(mo1lf.herb.liana, type = "III")

ggplot(total.lf, aes(x = log(liana + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")


# palmeiras
mo1lf.herb.palm <- glmer(herb ~ Treatment*log(palm+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.herb.palm)

car::Anova(mo1lf.herb.palm, type = "III")

ggplot(total.lf, aes(x = log(palm + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")


# arbustos
mo1lf.herb.shrub <- glmer(herb ~ Treatment*log(shrub+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.herb.shrub)

car::Anova(mo1lf.herb.shrub, type = "III")

ggplot(total.lf, aes(x = log(shrub + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2") + expand_limits(y=c(1,2.25))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")


# bamboo
mo1lf.herb.bamboo <- glmer(herb ~ Treatment*log(bamboo+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.herb.bamboo)

car::Anova(mo1lf.herb.bamboo, type = "III")

ggplot(total.lf, aes(x = log(bamboo + 1), y = log(herb + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Bamboos") + ylab("Herbs") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/herbsxbamboos.png", h = 15, w = 15, dpi = 300, units = "cm")


# grafico de significancias dos modelos

dwplot(list(mo1lf.herb.treat, mo1lf.herb.tree.sc, mo1lf.herb.liana.sc, mo1lf.herb.palm, mo1lf.herb.shrub, mo1lf.herb.bamboo),effect="fixed")+
  geom_vline(xintercept=0,lty=2) + theme_bw()








# bamboos -------------------------------------------------------------------------


## bamboos x tratamentos

mo1lf.bamboo.treat <- glmer(bamboo ~ Treatment + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.bamboo.treat)

car::Anova(mo1lf.bamboo.treat, type = "III")

ggplot(total.lf, aes(x = time, y = log(bamboo + 1), color = Treatment)) + geom_smooth(method = "lm")


## bamboos x grupos x tratamentos

# arvores
mo1lf.bamboo.tree <- glmer(bamboo ~  Treatment*log(tree+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.bamboo.tree)

car::Anova(mo1lf.bamboo.tree, type = "III")


ggplot(total.lf, aes(x = log(tree + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Trees") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxtrees.png", h = 15, w = 15, dpi = 300, units = "cm")


# lianas
mo1lf.bamboo.liana <- glmer(bamboo ~ Treatment*log(liana+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.bamboo.liana)

car::Anova(mo1lf.bamboo.liana, type = "III")

ggplot(total.lf, aes(x = log(liana + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Lianas") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxlianas.png", h = 15, w = 15, dpi = 300, units = "cm")


# palmeiras
mo1lf.bamboo.palm <- glmer(bamboo ~ Treatment*log(palm+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.bamboo.palm)


car::Anova(mo1lf.bamboo.palm, type = "III")

ggplot(total.lf, aes(x = log(palm + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Palms") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxpalms.png", h = 15, w = 15, dpi = 300, units = "cm")


# arbustos
mo1lf.bamboo.shrub <- glmer(bamboo ~ Treatment*log(shrub+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.bamboo.shrub)

car::Anova(mo1lf.bamboo.shrub, type = "III")


ggplot(total.lf, aes(x = log(shrub + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Shrubs") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxshrubs.png", h = 15, w = 15, dpi = 300, units = "cm")


# herbs
mo1lf.bamboo.herb <- glmer(bamboo ~  Treatment*log(herb+1) + (1 |Site/month ), family = poisson, data = total.lf)
summary(mo1lf.bamboo.herb)

car::Anova(mo1lf.bamboo.herb, type = "III")

ggplot(total.lf, aes(x = log(herb + 1), y = log(bamboo + 1), color = Treatment)) + 
  theme_classic() + 
  geom_smooth(method = "lm", linetype = "solid", alpha = 0.5, size = 2) + 
  xlab("Herbs") + ylab("Bamboos") +
  theme(axis.title = element_text(size = 36, face = "bold"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 32),
        legend.position = "none") + 
  scale_color_brewer(palette = "Dark2")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/GLMM/bamboosxherbs.png", h = 15, w = 15, dpi = 300, units = "cm")


# grafico de significancias dos modelos

dwplot(list(mo1lf.bamboo.treat, mo1lf.bamboo.tree, mo1lf.bamboo.liana, mo1lf.bamboo.palm, mo1lf.bamboo.shrub, mo1lf.bamboo.herb),effect="fixed")+
  geom_vline(xintercept=0,lty=2) + theme_bw()



# outros graficos-------------------------------------------------------------------------



trees_models <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/glmm/trees_glmm_summary.csv")
trees_models <- as_tibble(trees_models)
trees_models$term <- as.factor(trees_models$term)
trees_models
?geom_vline

?dwplot
dwplot(trees_models, dodge_size = .70, size = 5, 
       by_2sd = TRUE, 
       dot_args = list(size = 4, shape = 21, stroke = 1.7, fill = "grey80")) + 
  geom_vline(xintercept = 0, colour = "grey40", linetype = 5) + # plot line at zero _behind_ coefs
  theme_bw() +
  xlab("Coefficient Estimate") + ylab("Trees competitors") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.007, 0.65),
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5)

ggsave("C:/Users/Yuri/Desktop/aaa.png", width = 30, height = 10, units = "cm", dpi = 300)


