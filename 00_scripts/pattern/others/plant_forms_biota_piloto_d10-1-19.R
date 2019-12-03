### script para experimento piloto de formas de vida - BIOTA 10-1-2019
#Yuri

## como renomear itens dentro de uma coluna???
## como criar outra coluna e escolher uma sequencia para preencher???

library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")


rm(list = ls())

dados.biota <- read_csv("Life_Form_yuri_1-10-19.csv")
dados.biota

# sinstaxe do objeto dados - biota - removendo colunas - local - habito - numero do plot -  tratamento

d.b.rmc.num <- dados.biota %>% #seleciona apenas as colunas que contenham numeros. Como fazer isso dentro da funcao abaixo?
  select(c(7:21))
d.b.rmc.num

d.b.rmc <- dados.biota %>% #seleciona apenas as colunas que contenham dados e as colunas que vou usar
  select(c(1:20, 28)) %>% 
  mutate(abundance = rowSums(d.b.rmc.num))
d.b.rmc



str_trim(c(d.b.rmc$`Life Form`, d.b.rmc$Site, d.b.rmc$Treatment)) #remove os espacos #preciso atribuir a algum objeto???
str_to_lower(c(d.b.rmc$`Life Form`, d.b.rmc$Site, d.b.rmc$Treatment)) #remove maisuculas 
str(d.b.rmc)  

#is.na(d.b.rmc) # me mostra onde tem NA. Tem uma forma de so mostrar oque e falso e aonde?


#pegar primeiro oque for local, habito, numero do plot e tratamento

d.b.rmc.c.l <- d.b.rmc %>% 
  filter(`Site` == "CAR" & `Life Form` == "liana")
d.b.rmc.c.l

d.b.rmc.c.l.1.c <- d.b.rmc.c.l %>% 
  filter(`Site` == "CAR" & `Life Form` == "liana" & `Plot` == 1 & `Treatment` == "close")

d.b.rmc.c.l.1.c
teste <-plot(d.b.rmc.c.l.1.c$plot_novo, d.b.rmc.c.l.1.c$abundance)




summarise((d.b.rmc.c.l.1.c)) # pq nao vai????



d.b.rmc.c.l.1.o <- d.b.rmc.c.l %>% 
  filter(`Site` == "CAR" & `Life Form` == "liana" & `Plot` == 1 & `Treatment` == "open")
d.b.rmc.c.l.1.o

## outro jeito de fazer e pelo subset, mas tem que fazer um por um
#d.b.rmc <- subset(d.b.rmc, `Life Form` =="liana") # me retira apenas as lianas
#d.b.rmc

glimpse(d.b.rmc.c.l.1.c)

