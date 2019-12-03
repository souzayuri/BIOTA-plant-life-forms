### Yuri - 04-07-2019
### Script para calcular a proporcao de especies por area

rm(list = ls())

library(tidyverse)
library(dplyr)
library(plyr)
library(textclean)
library(beepr)
library(viridis)

ttlplt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
ttlplt[,23:24] <- lapply(ttlplt[,23:24], as.numeric)
ttlplt


#### proporcao de todas as especies

listaspcc <- ttlplt %>% 
  filter_row("Species", c("sp.", "cf.", "aff.", "Euterpe edulis")) %>% 
  select(-c(1,3,5:26,28))
listaspcc

listaspcc <- plyr::count(listaspcc$Species)
listaspcc

sum(listaspcc$freq)

proporcao <- listaspcc %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop)
proporcao
names(proporcao)[1] <- "Species"
proporcao

ggplot(data = proporcao, 
       aes(x = fct_reorder(Species, prop, .desc = TRUE),
           y = prop, fill = bolean)) +  theme_bw() +
  theme(legend.position="none") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("slateblue1","tomato")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=15)) + ylab("") + xlab("") +
  expand_limits(y=20) + scale_y_continuous(breaks=c(0,5,10,15,20))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_total.JPEG", he = 20, wi = 30, un = "cm", dpi = 300)



#### proporcao por area CAR

listaspcc <- ttlplt %>% 
  filter(`Site` == "CAR") %>% 
  filter_row("Species", c("sp.", "cf.", "aff.", "Euterpe edulis")) %>% 
  select(-c(1,3,5:26,28))
listaspcc
listaspcc <- plyr::count(listaspcc$Species)
listaspcc

proporcao <- listaspcc %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
proporcao

ggplot(data = proporcao, 
       aes(x = fct_reorder(x, prop, .desc = TRUE),
           y = prop, fill = bolean)) +  theme_bw() +
  theme(legend.position="none") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("tomato")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=30)) + ylab("") + xlab("") +
  expand_limits(y=20) + scale_y_continuous(breaks=c(0,5,10,15,20))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_total_CAR.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)



#### proporcao por area CBO

listaspcc <- ttlplt %>% 
  filter(`Site` == "CBO") %>% 
  filter_row("Species", c("sp.", "cf.", "aff.", "Euterpe edulis")) %>% 
  select(-c(1,3,5:26,28))
listaspcc
listaspcc <- plyr::count(listaspcc$Species)
listaspcc

proporcao <- listaspcc %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
proporcao

ggplot(data = proporcao, 
       aes(x = fct_reorder(x, prop, .desc = TRUE),
           y = prop, fill = bolean)) +  theme_bw() +
  theme(legend.position="none") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("tomato")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=30)) + ylab("") + xlab("") +
  expand_limits(y=20) + scale_y_continuous(breaks=c(0,5,10,15,20))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_total_CBO.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)




#### proporcao por area VGM

listaspcc <- ttlplt %>% 
  filter(`Site` == "VGM") %>% 
  filter_row("Species", c("sp.", "cf.", "aff.", "Euterpe edulis")) %>% 
  select(-c(1,3,5:26,28))
listaspcc
listaspcc <- plyr::count(listaspcc$Species)
listaspcc

proporcao <- listaspcc %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
proporcao

ggplot(data = proporcao, 
       aes(x = fct_reorder(x, prop, .desc = TRUE),
           y = prop, fill = bolean)) +  theme_bw() +
  theme(legend.position="none") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("tomato")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=30)) + ylab("") + xlab("") +
  expand_limits(y=20) + scale_y_continuous(breaks=c(0,5,10,15,20))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_total_VGM.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)




#### proporcao por area ITA

listaspcc <- ttlplt %>% 
  filter(`Site` == "ITA") %>% 
  filter_row("Species", c("sp.", "cf.", "aff.", "Euterpe edulis")) %>% 
  select(-c(1,3,5:26,28))
listaspcc
listaspcc <- plyr::count(listaspcc$Species)
listaspcc

proporcao <- listaspcc %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
proporcao

ggplot(data = proporcao, 
       aes(x = fct_reorder(x, prop, .desc = TRUE),
           y = prop, fill = bolean)) +  theme_bw() +
  theme(legend.position="none") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("tomato")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=30)) + ylab("") + xlab("") +
  expand_limits(y=20) + scale_y_continuous(breaks=c(0,10,20,30,40,50))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_total_ITA.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

