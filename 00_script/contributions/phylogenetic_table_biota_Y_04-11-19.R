
### Informations ------------------------------------------------------------

# title: Fixing and manipulating Biota data table ###
# author: Yuri (yuri.eco2013@gmail.com)
# data: 04/11/2019
# Description: This script creates a data table containing species abundances by plot, treatment and time, converting from widescreen to longscreen format, to be used in phylogenetic analyses



# Load packages and set directory -------------------------------------

rm(list = ls())

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("textclean")) install.packages("textclean", dependencies = TRUE)

path <- "C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions"
setwd(path)


# Load data table ----------------------------------------------------------

data_biota <- read_csv("biota_valesca_2019_ferns_v2.csv", locale = locale(encoding = "ASCII")) # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)


# Fix and manipulate data table ------------------------------------------------

data.biota.phyl <- data_biota %>%
  dplyr::select(-c(1,5,23:26,29)) %>% # remove unused columns
  gather(key = "Month", value = "value", 4:20) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  dplyr::group_by(Site, Treatment, Plot, Month, Species) %>% # create a group that will be used to calculate species abundances
  summarise(abundances = sum(value)) %>%  # create a 'abundances' column of each individual grouped by Site, Plot, Treatment and Month
  dplyr::mutate(Species = Species %>% 
           stringr::str_replace(" cf. ", " ")) %>% # this function substitute " cf. " for " " between Genus and species names. 
  dplyr::mutate(Species = Species %>% 
                  stringr::str_replace(" cf.", "")) %>% # this function remove the "cf." letters in the end of the name
  #mutate(species_new = Species %>% stringr::str_replace(" cf. ", " ")) %>% # It's possible to do the same thing using this other way, that creates a new column containing only species without " cf " between Genus and spp name. Then you can compare both columns. If you want to see this column on the final table, don't forget to include it in the last dplyr::select() function. 
  textclean::drop_row("Species", c("Indeterminate")) %>%  # this function removes all rows that contain "indeterminate" in the name, from the column "Species" or ("species_new").
  #textclean::drop_row("Species", c("sp.", "cf.", "aff.")) %>%  # this function removes all rows that contains "sp.", "cf." and/or "aff." in the name, from the column "Species" or ("species_new"). If you want to keep these rows, just comment this function line
                                                          # Paullinia/Serjania sp. became Paullinia sp.2, sp. 3, sp. 4 and sp. 5.
  dplyr::mutate(time = as.numeric(Month %>% stringr::str_replace("p", "")), 
         site = Site, treatment = Treatment, plot = Plot, month = Month) %>%  # this function creates a "time" column, with numerical values, using a character column called "month", and replicates these columns using the same name with capital first letters. 
  unite(Site, Site, Treatment, Plot, Month) %>% rename(PlotID = Site) %>% # join interest columns
  dplyr::select(site, treatment, plot, PlotID, Species, abundances, month, time) # organize by interest columns. If you change any column names above, don't forget to change it here, or include these other columns (like column new_species)
data.biota.phyl
glimpse(data.biota.phyl)


# extracting to a csv file ---------------------------------------------------

write_csv(data.biota.phyl, "biota_phylogenetic_datas_fixed.csv")

  

