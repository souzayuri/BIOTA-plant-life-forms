###
#title: Species matriz
#author: "Yuri Souza"
#data: "14/04/2020"
#content: Create a matriz of species abundance
###

rm(list = ls())



# load packages and table ----------------------------------------------------------

library(tidyverse)
library(textclean)
library(stringr)



data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1.csv", locale = locale(encoding = "ASCII")) # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)


# create matriz -----------------------------------------------------------


# table with all life forms -------------------------------------------------------------------


bt.spc.mtz.no.cf. <- data_biota %>% 
  select(-c(4,23:25,27:28)) %>% 
  gather(key = "Month", value = "value", 4:21) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  #dplyr::mutate(Species = Species %>% 
  #                stringr::str_replace(" cf. ", "..")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>%  # this function removes all rows that contains "sp.", "cf." and/or "aff." in the name, from the column "Species" or ("species_new"). If you want to keep these rows, just comment this function line
  mutate(site=Site, treatment = Treatment, plot = Plot, month = Month, time = Time) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::group_by(PlotID, site, treatment, plot, month, time, Species) %>%
  summarise(abundances = sum(value)) %>% 
  ungroup(PlotID, site, treatment, plot, month, time, species) %>%
  spread(Species, abundances) %>% 
  replace(is.na(.), 0)

bt.spc.mtz.no.cf.

write_csv(bt.spc.mtz.no.cf., "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/nacho_taxon_table_biota_no_cf._14-04-20.csv")




bt.spc.mtz.with.cf. <- data_biota %>% 
  select(-c(4,23:25,27:28)) %>% 
  gather(key = "Month", value = "value", 4:21) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf. ", " xx ")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>%  # this function removes all rows that contains "sp.", "cf." and/or "aff." in the name, from the column "Species" or ("species_new"). If you want to keep these rows, just comment this function line
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" xx ", " cf. ")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  mutate(site=Site, treatment = Treatment, plot = Plot, month = Month, time = Time) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::group_by(PlotID, site, treatment, plot, month, time, Species) %>%
  summarise(abundances = sum(value)) %>% 
  ungroup(PlotID, site, treatment, plot, month, time, species) %>%
  spread(Species, abundances) %>% 
  replace(is.na(.), 0)

bt.spc.mtz.with.cf.


write_csv(bt.spc.mtz.with.cf., "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/nacho_taxon_table_biota_with_cf.specie_14-04-20.csv")


