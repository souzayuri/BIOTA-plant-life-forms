### Yuri - 02-07-2019
### Script de boxplot do numero de especies em cada periodo de amostragem

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


#### numero de individuos de cada espécie
listaspcc <- ttlplt %>% 
  filter_row("Species", c("sp.", "cf.", "aff.", "Euterpe edulis")) %>% 
  select(-c(1,3,5:26,28))
listaspcc
listaspcc <- plyr::count(listaspcc$Species)
listaspcc

ggplot(data = listaspcc, 
       aes(x = fct_reorder(x, freq, .desc = TRUE),
           y = freq)) +  theme_classic() +
  theme(legend.position="top") + 
  geom_bar(stat = 'identity', colour="black", width = 0.55) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=12)) + ylab("") + xlab("") +
  expand_limits(y=1800) + scale_y_continuous(breaks=c(0,50,100,150,300,400,1800))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_total.JPEG", he = 20, wi = 30, un = "cm", dpi = 300)



#### Palmeiras
ttlplt_palm <- ttlplt %>% 
  filter(`Life Form` == "palm") %>% 
  filter_row("Species", c("sp.", "cf.","aff.", "Rubiaceae", "Myrtaceae")) %>% 
  mutate(Total = rowSums(.[6:22])) %>% 
  select(-c(1,3,5:26,28))
ttlplt_palm


ggplot(ttlplt_palm, aes(x= fct_reorder(Species, Total),
                        y = Total,
                        fill = Treatment)) + 
  geom_boxplot(width = 0.65) + coord_flip() + 
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), 
                    values=c("slateblue1","tomato")) + 
  theme_bw() + geom_jitter(width=0.1,alpha=0.2) + ggtitle("Palms") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        #panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        #legend.position = c(.95, .95),
        #legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  + 
  labs(x = "Species", y = "Time")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_tempo_palm.JPEG", he = 20, wi = 20, un = "cm", dpi = 300)


#### árvores

ttlplt_tree <- ttlplt %>% 
  filter(`Life Form` == "tree") %>% 
  filter_row("Species", c("sp.", "cf.","aff.")) %>% 
  mutate(Total = rowSums(.[6:22])) %>% 
  select(-c(1,3,5:26,28))
ttlplt_tree

#listaspcc <- plyr::count(ttlpltrm$Species)
#listaspcc

ggplot(ttlplt_tree, aes(x= fct_reorder(Species, Total),
                        y = Total,
                        fill = Treatment)) + 
  geom_boxplot(width = 0.65) + coord_flip() + 
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), 
                    values=c("slateblue1","tomato")) + 
  theme_bw() + geom_jitter(width=0.1,alpha=0.2) + ggtitle("Trees") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        #panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        #legend.position = c(.95, .95),
        #legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  + 
  labs(x = "Species", y = "Time")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_tempo_tree.JPEG", he = 30, wi = 20, un = "cm", dpi = 300)


#### Ervas

ttlplt_herb <- ttlplt %>% 
  filter(`Life Form` == "herb") %>% 
  filter_row("Species", c("sp.", "cf.","aff.")) %>% 
  mutate(Total = rowSums(.[6:22])) %>% 
  select(-c(1,3,5:26,28))
ttlplt_herb

#listaspcc <- plyr::count(ttlpltrm$Species)
#listaspcc

ggplot(ttlplt_herb, aes(x= fct_reorder(Species, Total),
                        y = Total,
                        fill = Treatment)) + 
  geom_boxplot(width = 0.65) + coord_flip() + 
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), 
                    values=c("slateblue1","tomato")) + 
  theme_bw() + geom_jitter(width=0.1,alpha=0.2) + ggtitle("Herbs") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        #panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        #legend.position = c(.95, .95),
        #legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  + 
  labs(x = "Species", y = "Time")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_tempo_herb.JPEG", he = 20, wi = 20, un = "cm", dpi = 300)



#### Lianas

ttlplt_liana <- ttlplt %>% 
  filter(`Life Form` == "liana") %>% 
  filter_row("Species", c("sp.", "cf.","aff.")) %>% 
  mutate(Total = rowSums(.[6:22])) %>% 
  select(-c(1,3,5:26,28))
ttlplt_liana

#listaspcc <- plyr::count(ttlpltrm$Species)
#listaspcc

ggplot(ttlplt_liana, aes(x= fct_reorder(Species, Total),
                        y = Total,
                        fill = Treatment)) + 
  geom_boxplot(width = 0.65) + coord_flip() + 
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), 
                    values=c("slateblue1","tomato")) + 
  theme_bw() + geom_jitter(width=0.1,alpha=0.2) + ggtitle("Lianas") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        #panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        #legend.position = c(.95, .95),
        #legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  + 
  labs(x = "Species", y = "Time")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_tempo_liana.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)



#### Bamboos

ttlplt_bamboo <- ttlplt %>% 
  filter(`Life Form` == "bamboo") %>% 
#  filter_row("Species", c("sp.", "cf.")) %>% 
  mutate(Total = rowSums(.[6:22])) %>% 
  select(-c(1,3,5:26,28))
ttlplt_bamboo

#listaspcc <- plyr::count(ttlpltrm$Species)
#listaspcc

ggplot(ttlplt_bamboo, aes(x= fct_reorder(Species, Total),
                         y = Total,
                         fill = Treatment)) + 
  geom_boxplot(width = 0.65) + coord_flip() + 
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), 
                    values=c("slateblue1","tomato")) + 
  theme_bw() + geom_jitter(width=0.1,alpha=0.2) + ggtitle("Bamboos") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        #panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        #legend.position = c(.95, .95),
        #legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  + 
  labs(x = "Species", y = "Time")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_tempo_bamboo.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)



#### Arbusto

ttlplt_shrub <- ttlplt %>% 
  filter(`Life Form` == "shrub") %>% 
  filter_row("Species", c("sp.", "cf.","aff.")) %>% 
  mutate(Total = rowSums(.[6:22])) %>% 
  select(-c(1,3,5:26,28))
ttlplt_shrub

#listaspcc <- plyr::count(ttlpltrm$Species)
#listaspcc

ggplot(ttlplt_shrub, aes(x= fct_reorder(Species, Total),
                          y = Total,
                          fill = Treatment)) + 
  geom_boxplot(width = 0.65) + coord_flip() + 
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), 
                    values=c("slateblue1","tomato")) + 
  theme_bw() + geom_jitter(width=0.1,alpha=0.2) + ggtitle("Shrubs") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        #panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        #legend.position = c(.95, .95),
        #legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  + 
  labs(x = "Species", y = "Time")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_tempo_shrub.JPEG", he = 20, wi = 20, un = "cm", dpi = 300)



#### Todas as especies e formas de vida
ttlplt_species <- ttlplt %>% 
  filter_row("Species", c("sp.", "cf.","aff.")) %>% 
  mutate(Total = rowSums(.[6:22])) %>% 
  select(-c(1,3,5:26,28,29))
ttlplt_species


ggplot(ttlplt_species, aes(x= fct_reorder(Species, Total),
                        y = Total,
                        fill = Treatment)) + 
  geom_boxplot(width = 0.65) + coord_flip() + 
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), 
                    values=c("slateblue1","tomato")) + 
  theme_bw() + geom_jitter(width=0.1,alpha=0.2) + 
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        #panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        #legend.position = c(.95, .95),
        #legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  + 
  labs(x = "Species", y = "Time")

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/especies_tempo/especies_tempo_todas.JPEG", he = 80, wi = 20, un = "cm", dpi = 300)


#beep(2)




