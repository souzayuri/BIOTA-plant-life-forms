### title: facetwrap abundancia ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 10/14/2019
### Description: grafico de abundancia por grupo e tempo atraves do facet_wrap


rm(list = ls()) 

library(tidyverse)
library(stringr)
library(boot)


### Fitting the data

bt.dt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
bt.dt 

bt.dt.slc <- bt.dt [,-c(1,5,23:28)]
bt.dt.slc

bt.dt.slc.gtr <- bt.dt.slc %>% 
  gather(key = "time", value = "value", 4:20)
bt.dt.slc.gtr

total.lf <- bt.dt.slc.gtr %>% 
  group_by(Site, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = sum(value)) %>% 
  rename(life_form = `Life Form`, month = time) %>% # renomeia a coluna `Life Form`
  mutate(time = as.numeric(month %>% stringr::str_replace("p", ""))) %>% 
  filter(life_form != "indeterminate")
total.lf


ggplot(total.lf, aes(time, abundancia, fill = Treatment)) +
  geom_bar(stat='identity', position='dodge') + 
  scale_fill_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + 
  theme_bw() + 
  theme(strip.background = element_rect(
    color="black", fill="gray88", size=0,8, linetype="solid"),
    legend.position = c(0.10, .90)) + labs(x = "Time (months)", y = "Abundances") +
  facet_wrap(~life_form)


ggsave("C:/Users/Layu/Downloads/facet_wrap_life_form.png", h = 15, w = 15, dpi = 300, units = "cm")

