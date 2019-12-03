library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")
library("tidyquant")
library("cranlogs")

rm(list = ls()) #limpa a memoria

d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv")


### Graficos de Abundancia


# lianas

d.b.car.l <- d.b %>% 
  filter(`Life_Form` == "liana" & `value` >= 0 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site) %>% 
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment) %>% 
  arrange(year)


ggplot(d.b.car.l, aes(x=year, y=abundance, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("brown", "tan")) + theme_bw() +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Lianas") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# arvores

d.b.car.t <- d.b %>% 
  filter(`Life_Form` == "tree" & `value` >= 0 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site) %>% 
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment) %>% 
  arrange(year)


ggplot(d.b.car.t, aes(x=year, y=abundance, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("sienna","olivedrab")) + theme_bw() +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Trees") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# ervas

d.b.car.h <- d.b %>% 
  filter(`Life_Form` == "herb" & `value` >= 0 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site) %>% 
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment) %>% 
  arrange(year)


ggplot(d.b.car.h, aes(x=year, y=abundance, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("firebrick3","salmon1")) + theme_bw() +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Herbs") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# bamboo

d.b.car.b <- d.b %>% 
  filter(`Life_Form` == "bamboo" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site) %>% 
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment) %>% 
  arrange(year)


ggplot(d.b.car.b, aes(x=year, y=abundance, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("orange4","orangered")) + theme_bw() +
  labs(x = "Time Series",
       y = "Abundance",
       title = "bamboo") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# Palm

d.b.car.p <- d.b %>% 
  filter(`Life_Form` == "palm" & `value` >= 0 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site) %>% 
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment) %>% 
  arrange(year)


ggplot(d.b.car.p, aes(x=year, y=abundance, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("slateblue","turquoise4")) + theme_bw() +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Palm") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))





### Grafico da media

library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")
library("tidyquant")
library("cranlogs")


# Palm

d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv")
d.b.car.p.m <- d.b %>% 
  filter(`Life_Form` == "palm" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site, value, Plot) %>% 
  summarise(abundance = sum(value)) %>%
  summarise(plot_w_spp = sum(value), abundance_ttl = sum(abundance)) %>%
  summarise(mean_spp =  abundance_ttl/plot_w_spp) %>%
  arrange(Treatment) %>% 
  arrange(year)
d.b.car.p.m


ggplot(d.b.car.p.m, aes(x=year, y=mean_spp, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("slateblue","turquoise4")) + theme_bw() +
  labs(x = "Time Series",
       y = "Mean Abundance",
       title = "Palm") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# liana

d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv")
d.b.car.l.m <- d.b %>% 
  filter(`Life_Form` == "liana" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site, value, Plot) %>% 
  summarise(abundance = sum(value)) %>% 
  summarise(plot_w_spp = sum(value), abundance_ttl = sum(abundance)) %>%
  summarise(mean_spp =  abundance_ttl/plot_w_spp) %>%
  arrange(Treatment) %>% 
  arrange(year)
d.b.car.l.m

ggplot(d.b.car.l.m, aes(x=year, y=mean_spp, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("brown", "tan")) + theme_bw() +
  labs(x = "Time Series",
       y = "Mean Abundance",
       title = "Liana") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# Tree

d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv")
d.b.car.t.m <- d.b %>% 
  filter(`Life_Form` == "tree" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site, value, Plot) %>% 
  summarise(abundance = sum(value)) %>% 
  summarise(plot_w_spp = sum(value), abundance_ttl = sum(abundance)) %>%
  summarise(mean_spp =  abundance_ttl/plot_w_spp) %>%
  arrange(Treatment) %>% 
  arrange(year)
d.b.car.t.m

ggplot(d.b.car.t.m, aes(x=year, y=mean_spp, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("sienna","olivedrab")) + theme_bw() +
  labs(x = "Time Series",
       y = "Mean Abundance",
       title = "Tree") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# Bamboo

d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv")
d.b.car.b.m <- d.b %>% 
  filter(`Life_Form` == "bamboo" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site, value, Plot) %>% 
  summarise(abundance = sum(value)) %>% 
  summarise(plot_w_spp = sum(value), abundance_ttl = sum(abundance)) %>%
  summarise(mean_spp =  abundance_ttl/plot_w_spp) %>%
  arrange(Treatment) %>% 
  arrange(year)
d.b.car.b.m

ggplot(d.b.car.b.m, aes(x=year, y=mean_spp, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("orange4","orangered")) + theme_bw() +
  labs(x = "Time Series",
       y = "Mean Abundance",
       title = "Bamboo") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


# Herbs

d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv")
d.b.car.h.m <- d.b %>% 
  filter(`Life_Form` == "herb" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site, value, Plot) %>% 
  summarise(abundance = sum(value)) %>% 
  summarise(plot_w_spp = sum(value), abundance_ttl = sum(abundance)) %>%
  summarise(mean_spp =  abundance_ttl/plot_w_spp) %>%
  arrange(Treatment) %>% 
  arrange(year)
d.b.car.h.m

ggplot(d.b.car.h.m, aes(x=year, y=mean_spp, color = Treatment)) + 
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("firebrick3","salmon1")) + theme_bw() +
  labs(x = "Time Series",
       y = "Mean Abundance",
       title = "Herbs") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))



#### ANOVA

#herbs
ajuste <- lm(d.b.car.h.m$mean_spp~d.b.car.h.m$Treatment)
ajuste
summary(ajuste)
anova(ajuste)


#Lianas
ajuste <- lm(d.b.car.l.m$mean_spp~d.b.car.l.m$Treatment)
ajuste
summary(ajuste)
anova(ajuste)

#Tree
ajuste <- lm(d.b.car.t.m$mean_spp~d.b.car.t.m$Treatment)
ajuste
summary(ajuste)
anova(ajuste)

#palm
ajuste <- lm(d.b.car.p.m$mean_spp~d.b.car.p.m$Treatment)
ajuste
summary(ajuste)
anova(ajuste)

#bamboo
ajuste <- lm(d.b.car.b.m$mean_spp~d.b.car.b.m$Treatment)
ajuste
summary(ajuste)
anova(ajuste)





### Desvio padrão


d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv")
d.b.car.h.sd <- d.b %>% 
  filter(`Life_Form` == "herb" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(year, Treatment, Site, value, Plot) %>% 
  summarise(abundance = sum(value)) %>% 
  summarise(plot_w_spp = sum(value), abundance_ttl = sum(abundance)) %>%
  summarise(mean_spp =  abundance_ttl/plot_w_spp) %>%
  arrange(Treatment) %>% 
  arrange(year)
d.b.car.h.sd

ggplot(d.b.car.h.sd, aes(x=year, y=mean_spp, color = Treatment)) + 
  geom_ribbon(aes(ymin = mean_spp - 1, ymax = mean_spp + 1), fill = "grey70") +
  geom_line(size = 0.7) + geom_jitter(aes(color = Treatment), width = .3, size = 3, alpha = .7) +  
  facet_wrap(~Site, scales="free_y") + scale_color_manual(values=c("slateblue","turquoise4")) + theme_bw() +
  labs(x = "Time Series",
       y = "Mean Abundance",
       title = "Herbs") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))



