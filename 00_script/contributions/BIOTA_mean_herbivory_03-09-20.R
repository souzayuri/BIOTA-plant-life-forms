# Informations ------------------------------------------------------------

### title: Herbivory ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 03/09/2020
### Description: This script creates a data table containing mean of leaf herbivory by plot and treatment



# Load packages and set directory -------------------------------------

rm(list = ls())

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("naniar")) install.packages("naniar", dependencies = TRUE)

# load data table ----------------------------------------------------------

data_biota.herb <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/herbivoria_media_individual_natalia.csv")
glimpse(data_biota.herb)


data_biota.herb.ttl <- data_biota.herb %>%
  replace(is.na(.), 0) %>% 
  dplyr::select(-c(4:7,9,11,13,15,17)) %>% 
  #mutate(GAL = av_GAL) %>% 
  #dplyr::select(-c(9,10)) %>%
  #mutate_at(9, funs(round(., ))) %>%
  group_by(local, plot, treatment) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_at(4:9, funs(round(., ))) # %>% 
  #naniar::replace_with_na(replace = list(leaf_damage = 0))
data_biota.herb.ttl  

write_csv(data_biota.herb.ttl, "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_biota.herb.ttl_03-09-20.csv")
