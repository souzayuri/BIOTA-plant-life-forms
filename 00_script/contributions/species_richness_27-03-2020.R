
# Informations ------------------------------------------------------------

### title: Species richness by plot ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 27/03/2020
### Description: This script calculate species richness by repeated names and eveness




# Load packages and set directory -------------------------------------

rm(list = ls())

library(tidyverse)
library(codyn)

ttlplt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.csv")
ttlplt



# Species richness --------------------------------------------------------

listaspcc <- ttlplt %>%
  select(c(1:3,26)) %>% 
  group_by(Site, Plot, Treatment, Species) %>%  
  summarise(Freq=n()) %>% 
  ungroup(Site, Plot, Treatment, Species) %>% 
  group_by(Site, Plot, Treatment) %>%
  summarise(Freq=n())
listaspcc

write_csv(listaspcc, "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/species_richness_03-09-20.csv")



# Species evenness --------------------------------------------------------

#CAR

listaspcc.car <- ttlplt %>%
  select(c(1:3,26)) %>% 
  group_by(Site, Plot, Treatment, Species) %>%  
  summarise(Freq=n()) %>% 
  filter(Site == "CAR")
listaspcc.car


car <- community_structure(listaspcc.car,
                    time.var="Plot",
                    replicate.var = "Treatment",
                    abundance.var = "Freq",
                    metric = "SimpsonEvenness") # for Simpson's evenness measure
car$site <- rep("CAR")
car

#CBO

listaspcc.cbo <- ttlplt %>%
  select(c(1:3,26)) %>% 
  group_by(Site, Plot, Treatment, Species) %>%  
  summarise(Freq=n()) %>% 
  filter(Site == "CBO")
listaspcc.cbo


cbo <- community_structure(listaspcc.cbo,
                    time.var="Plot",
                    replicate.var = "Treatment",
                    abundance.var = "Freq",
                    metric = "SimpsonEvenness") # for Simpson's evenness measure
cbo$site <- rep("CBO")
cbo


#VGM

listaspcc.vgm <- ttlplt %>%
  select(c(1:3,26)) %>% 
  group_by(Site, Plot, Treatment, Species) %>%  
  summarise(Freq=n()) %>% 
  filter(Site == "VGM")
listaspcc.vgm


vgm <- community_structure(listaspcc.vgm,
                    time.var="Plot",
                    replicate.var = "Treatment",
                    abundance.var = "Freq",
                    metric = "SimpsonEvenness") # for Simpson's evenness measure

vgm$site <- rep("VGM")
vgm



#ITA

listaspcc.ita <- ttlplt %>%
  select(c(1:3,26)) %>% 
  group_by(Site, Plot, Treatment, Species) %>%  
  summarise(Freq=n()) %>% 
  filter(Site == "ITA")
listaspcc.ita


ita <- community_structure(listaspcc.ita,
                    time.var="Plot",
                    replicate.var = "Treatment",
                    abundance.var = "Freq",
                    metric = "SimpsonEvenness") # for Simpson's evenness measure

ita$site <- rep("ITA")
ita

evenness <- bind_rows(cbo, car, vgm, ita) %>% 
  select(5,1:4)

evenness

write.csv(evenness, "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/species_evenness_03-27-20.csv", row.names = FALSE, quote = TRUE) #row names cria uma coluna com o level

