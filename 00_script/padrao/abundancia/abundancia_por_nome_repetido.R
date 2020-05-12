### Yuri 23-4-2019
# Script para calcular a abudancia das especies para areas de interesse

#install.packages("tidyverse")

library(tidyverse)

spp <- as.data.frame(read.csv("Life_Form_yuri_13-1-19.csv", header = T))
spp

spp <- spp  %>% 
  filter( Site == "CBO" & `Plot` == 1 & `Treatment` == "close" & `Trecruit` == "T0")
spp


listaspc <- plyr::count(spp$Species)

#listaspc <- listaspc[order(listaspc$freq),] 
listaspc
