# Informations ------------------------------------------------------------

### title: Herbivory - individual's identification ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 03/09/2020
### Description: This script fill the herbivory datatable with the individuals' species names, family and life-forms


# Load packages and set directory -------------------------------------

rm(list = ls())

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)

data_biota.herbivory <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/herbivoria_media_individual_natalia.csv")
data_biota.herbivory

data_biota.species <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.csv")
data_biota.species


# match tables ------------------------------------------------------------

# species
data_biota.herbivory$Species <- data_biota.species$Species[match(data_biota.herbivory$ID_plant, data_biota.species$Individual)]

data_biota.herbivory_species <- data_biota.herbivory %>% 
  select(1:4,19,5:18)
data_biota.herbivory_species

### family
data_biota.herbivory_species$family <- data_biota.species$Family[match(data_biota.herbivory_species$Species, data_biota.species$Species)]

data_biota.herbivory_species_family <- data_biota.herbivory_species %>% 
  select(1:5,20,6:19)
data_biota.herbivory_species_family

### life form
data_biota.herbivory_species_family$life_form <- data_biota.species$`Life Form`[match(data_biota.herbivory_species_family$Species, data_biota.species$Species)]

data_biota.herbivory_species_family_life_forms <- data_biota.herbivory_species_family %>% 
  select(1:6,21,7:20)
data_biota.herbivory_species_family_life_forms



write_csv(data_biota.herbivory_species_family_life_forms, "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_biota.herbivory_species_family_life_forms.csv")




