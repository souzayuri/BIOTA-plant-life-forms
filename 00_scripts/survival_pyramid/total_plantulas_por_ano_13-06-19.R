### Script para contagem da piramide etaria
## Yuri 6-5-19

library(plotrix)
library(tidyverse)

gf <- read.csv("Life_Form_yuri_13-1-19_v2na.csv")
gf <- gf[,-c(1,3,5,21:27)]

gf$sum <- rowSums(gf[,3:16]) # criando uma nova coluna com as somas dos periodos de amostragens
gf


### Calcular a frequencia de todas as plantulas aparecem em cada tempo e tratamento
gffreqc <- gf  %>% 
  filter(`Site` == "VGM", `Treatment` == "close")
gffreqc  <- plyr::count(gffreqc$sum)
gffreqc



gf <- read.csv("Life_Form_yuri_13-1-19_v2na.csv")
gf <- gf[,-c(1,3,5,21:27)]

gf$sum <- rowSums(gf[,3:16])
gf

gffreqc <- gf  %>% 
  filter(`Site` == "VGM", `Treatment` == "open")
gffreqc  <- plyr::count(gffreqc$sum)
gffreqc




