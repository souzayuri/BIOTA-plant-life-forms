

### script para experimento piloto de formas de vida - BIOTA 10-1-2019
#Yuri

## como renomear itens dentro de uma coluna???
## como criar outra coluna e escolher uma sequencia para preencher???

library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")

rm(list = ls())

dados.biota <- read_csv("Life_Form_yuri_1-10-19.csv")
dados.biota

#vê se por esse caminho funciona. de qlqr forma, é a função gather que vc deve querer, seguida da separate pra se livrar do "t".

dados.biota_2 <- dados.biota %>% 
  # ele coleta as colunas de t1:t17 e as agregas na coluna "time"
  gather(key = "time", value = "value", 6:22)
  # aqui separo o t dos numeros
 # separate(time, into = c("t", "n"), sep = "t") %>% 
  # e aqui seleciono só as colunas com ID, nº do tempo e o valor
#  select(ID, n, value)
# daí aqui só agrego

dados.biota_2$Index <- 1:nrow(dados.biota_2)
dados.biota_2

biota_sample <- dados.biota_2[sample(1:nrow(dados.biota_2), 10),] #faz uma amostragem "sample" aleatorio das linhas. vai da primeira a ultima linha do df "1:nrow(df)" e pega 3 numeros aleatorios
biota_sample


write.csv(dados.biota_2, "planilha_biota_time_ind_R.csv", row.names = FALSE, quote = TRUE) #row names cria uma coluna com o level
