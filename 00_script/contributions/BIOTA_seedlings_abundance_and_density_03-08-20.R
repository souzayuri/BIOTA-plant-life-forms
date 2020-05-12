# Informations ------------------------------------------------------------

### title: Abundance and density of seedlings ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 03/09/2020
### Description: This script creates a data table containing abundance and density of seedling in each BIOTA plots



# Load packages and set directory -------------------------------------

rm(list = ls())

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("textclean")) install.packages("textclean", dependencies = TRUE)

path <- "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao"
setwd(path)




# load data table ----------------------------------------------------------

data_biota <- read_csv("life_form_yuri_2020v1.csv", locale = locale(encoding = "ASCII"))
glimpse(data_biota)


# abundance and density ---------------------------------------------------------------


data.biota.abun.den <- data_biota %>%
  dplyr::select(-c(5:28)) %>%
  #gather(key = "Month", value = "value", 5:21) %>% 
  #group_by(Site, Plot, Treatment) %>% 
  #summarise(abundances = sum(value)) %>% 
  group_by(Site, Plot, Treatment) %>%
  summarise(abundance=n()) %>% 
  mutate(density = abundance/3^2)

data.biota.abun.den

write_csv(data.biota.abun.den, "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/BIOTA_density_abundance-03-09-20.csv")
