# Informations ------------------------------------------------------------

### title: Herbivory ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 03/09/2020
### Description: This script creates a data table containing mean of leaf herbivory by plot and treatment



# Load packages and set directory -------------------------------------

rm(list = ls())

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)


# load datatable ----------------------------------------------------------


data_biota.gas <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/dados_GHG.csv")
glimpse(data_biota.gas)



# preparing data - mean soil gases ----------------------------------------------------------


data_biota.gas.mean <- data_biota.gas %>%
  group_by(ID, Soil) %>% 
  summarise_all(funs(mean))

data_biota.gas.mean 

write_csv(data_biota.gas.mean, "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/CAR_dados_GHG_mean_03-09-20.csv")
