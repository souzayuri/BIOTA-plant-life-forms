### Script para contagem da piramide etaria
## Yuri 6-5-19

library(plotrix)
library(tidyverse)

gf <- read.csv("Life_Form_yuri_13-1-19_v2na.csv")
gf <- gf[,-c(5,7:9,11:13,15:17,19,21:24)]

gf$sum <- rowSums(gf[,5:9]) # criando uma nova coluna com as somas dos periodos de amostragens
gf


### Calcular a frequencia de vezes que as plantulas aparecem em cada tempo para Ervas em cada tratamento
gffreqherbc <- gf  %>% 
  filter(`Treatment` == "close"	& `Life_Form` == "herb" )
gffreqherbc  <- plyr::count(gffreqherbc$sum)
gffreqherbc 
gffreqherbcp <- gffreqherbc*100/sum((gffreqherbc$freq))
gffreqherbcp


gffreqherbo <- gf  %>% 
  filter(`Treatment` == "open"	& `Life_Form` == "herb" )
gffreqherbo  <- plyr::count(gffreqherbo$sum)
gffreqherbo 
gffreqherbop <- gffreqherbo*100/sum((gffreqherbo$freq))
gffreqherbop


### Calcular a frequencia de vezes que as plantulas aparecem em cada tempo para lianas em cada tratamento
gffreqlianc <- gf  %>% 
  filter(`Treatment` == "close"	& `Life_Form` == "liana" )
gffreqlianc  <- plyr::count(gffreqlianc$sum)
gffreqlianc
gffreqliancp <- gffreqlianc*100/sum((gffreqlianc$freq))
gffreqliancp


gffreqliano <- gf  %>% 
  filter(`Treatment` == "open"	& `Life_Form` == "liana" )
gffreqliano  <- plyr::count(gffreqliano$sum)
gffreqliano 
gffreqlianop <- gffreqliano*100/sum((gffreqliano$freq))
gffreqlianop


### Calcular a frequencia de vezes que as plantulas aparecem em cada tempo para trees em cada tratamento
gffreqtreec <- gf  %>% 
  filter(`Treatment` == "close"	& `Life_Form` == "tree" )
gffreqtreec  <- plyr::count(gffreqtreec$sum)
gffreqtreec 
gffreqtreecp <- gffreqtreec*100/sum((gffreqtreec$freq))
gffreqtreecp

gffreqtreeo <- gf  %>% 
  filter(`Treatment` == "open"	& `Life_Form` == "tree" )
gffreqtreeo  <- plyr::count(gffreqtreeo$sum)
gffreqtreeo
gffreqtreeop <- gffreqtreeo*100/sum((gffreqtreeo$freq))
gffreqtreeop


### Calcular a frequencia de vezes que as plantulas aparecem em cada tempo para trees em cada tratamento
gffreqpalmc <- gf  %>% 
  filter(`Treatment` == "close"	& `Life_Form` == "palm" )
gffreqpalmc  <- plyr::count(gffreqpalmc$sum)
gffreqpalmc 
gffreqpalmcp <- gffreqpalmc*100/sum((gffreqpalmc$freq))
gffreqpalmcp

gffreqpalmo <- gf  %>% 
  filter(`Treatment` == "open"	& `Life_Form` == "palm" )
gffreqpalmo  <- plyr::count(gffreqpalmo$sum)
gffreqpalmo 
gffreqpalmop <- gffreqpalmo*100/sum((gffreqpalmo$freq))
gffreqpalmop

### Calcular a frequencia de vezes que as plantulas aparecem em cada tempo para bamboos em cada tratamento
gffreqbambc <- gf  %>% 
  filter(`Treatment` == "close"	& `Life_Form` == "bamboo" )
gffreqbambc  <- plyr::count(gffreqbambc$sum)
gffreqbambc 
gffreqbambcp <- gffreqbambc*100/sum((gffreqbambc$freq))
gffreqbambcp

gffreqbambo <- gf  %>% 
  filter(`Treatment` == "open"	& `Life_Form` == "bamboo" )
gffreqbambo  <- plyr::count(gffreqbambo$sum)
gffreqbambo 
gffreqbambop <- gffreqbambo*100/sum((gffreqbambo$freq))
gffreqbambop


### Calcular a frequencia de todas as plantulas aparecem em cada tempo e tratamento
gffreqc <- gf  %>% 
  filter(`Treatment` == "close")
gffreqc  <- plyr::count(gffreqc$sum)
gffreqc
gffreqc <- gffreqc*100/sum((gffreqc$freq))
gffreqc


gffreqo <- gf  %>% 
  filter(`Treatment` == "open")
gffreqo  <- plyr::count(gffreqo$sum)
gffreqo 
gffreqo <- gffreqo*100/sum((gffreqo$freq))
gffreqo




