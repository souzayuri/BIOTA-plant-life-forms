### title: Mammals biomass and trampling ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 03/04/2020
### Description: Creates a table containing mean informations about mammals biomass and trampling in BIOTA plots


# Load packages and table -------------------------------------------------


rm(list = ls())


library(tidyverse)
library(textclean)

mammals_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/Mammals_BIOTA_PLOTS.csv")
mammals_biota


# Mammals biomass and trampling  --------------------------------------------------------


mammals_biota_total <- mammals_biota %>% 
  replace(is.na(.), 0) %>% 
  select(c(1,3:17)) %>% 
  mutate(efforts_hours = effort_days*24) %>%
  group_by(SITE, PLOT, GRID) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup(SITE, PLOT, GRID) %>% 
  mutate(BM_Medium_Large_g = rowSums(.[5:8])) %>% 
  mutate(Medium_Large_trampling_day = rowSums(.[12:15])) %>% 
  mutate(BM_TOTAL_g = log(BM_TOTAL_g+1),
         BM_Medium_Large_g = log(BM_Medium_Large_g+1),
         BM_small_g = log(BM_small_g+1),
         total_trampling_day = log(total_trampling_day+1),
         Medium_Large_trampling_day = log(Medium_Large_trampling_day+1),
         SMALL_trampling_day = log(SMALL_trampling_day+1)) %>% 
  select(SITE,PLOT,GRID,efforts_hours,BM_TOTAL_g,
         BM_Medium_Large_g,BM_small_g,total_trampling_day,
         Medium_Large_trampling_day,SMALL_trampling_day
         )
mammals_biota_total

write_csv(mammals_biota_total,"C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/Mammals_BIOTA_PLOTS_mauro_table.csv")



# preparing table ---------------------------------------------------------


#mammals_biota.car <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_mammals_biota_2017_car.csv")
#mammals_biota.cbo <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_mammals_biota_2017_cb.csv")
#mammals_biota.vgm <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_mammals_biota_2017_vg.csv")
#mammals_biota.ita <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_mammals_biota_2017_sv.csv")


#data.biota.mammals.spp <- rbind(mammals_biota.car,mammals_biota.cbo,mammals_biota.vgm,mammals_biota.ita) %>% 
#  select(c(1,2,4,9,11)) %>% 
#  dplyr::mutate(type = type %>%  stringr::str_replace(" ", ""),
#                type = type %>%  stringr::str_replace("Bird", "bird")) %>% 
#  textclean::drop_row("type", c("bird"))
#data.biota.mammals.spp

#write_csv(data.biota.mammals.spp,"C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_mammals_spp-04-03-20.csv")


# richness ----------------------------------------------------------------

mammals_biota.spp <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/data_mammals_spp-04-03-20.csv")


data.biota.spp <- mammals_biota.spp %>%
  dplyr::mutate(Parcelas = Parcelas %>%  stringr::str_replace("F", ""),
                Parcelas = Parcelas %>%  stringr::str_replace("A", ""),
                Parcelas = Parcelas %>%  stringr::str_replace("P", ""),
                Parcelas = Parcelas %>%  stringr::str_replace(" ", ""),
                grid = grid %>%  stringr::str_replace("close", "Closed"),
                grid = grid %>%  stringr::str_replace("closed", "Closed"),
                grid = grid %>%  stringr::str_replace("open", "Open"),
                grid = grid %>%  stringr::str_replace(" ", ""),
                base = base %>%  stringr::str_replace("iC", "IC"),
                base = base %>%  stringr::str_replace(" ", "")) %>%
  group_by(base, Parcelas, grid, species) %>% 
  summarise(Freq=n()) %>% 
  ungroup(base, Parcelas, grid, species) %>% 
  group_by(base, Parcelas, grid) %>%
  summarise(Freq=n())
data.biota.spp



write_csv(data.biota.spp,"C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/contributions/biota.plot_mammals_spp-04-03-20.csv")
