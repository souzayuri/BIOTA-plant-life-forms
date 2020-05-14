###
#title: Inverse Simpson eveness
#author: "Yuri Souza"
#data: "05/05/2020"
#content: Create a life-form species matriz to calculate inverse simpson diversity to apply to pielous eveness
###

rm(list = ls())



# load packages and table ----------------------------------------------------------


library(tidyverse)
library(hillR)
library(hablar)
library(stringr)
library(lme4) 
library(car)
library(naniar)
library(ggpubr)
library(grid)
library(codyn)
library(rcompanion)
library(MASS)
library(lmtest)



data_biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1_para_abundancia.csv") # If all the columns is imported as agregated, try to load with read_csv2 function 
glimpse(data_biota)


# table with inverse simpsons calculated  -------------------------------------------------------------------


bt.invsim.mtz <- data_biota %>% 
  rename(life_form = `Life Form`) %>% 
  filter(!life_form == "indeterminate", !life_form == "arborescent fern") %>% 
  dplyr::select(-c(4, 24:26,28)) %>% 
  gather(key = "Month", value = "value", 4:22) %>% 
  dplyr::mutate(Time = as.numeric(Month %>% stringr::str_replace("T", ""))) %>% 
  mutate(site=Site, treatment = Treatment, plot = Plot, month = Month, time = Time, lifeform = life_form) %>% 
  unite(Site, Site, Treatment, Plot, Month, Time) %>% rename(PlotID = Site) %>% 
  dplyr::group_by(PlotID, site, treatment, plot, month, time, lifeform) %>%
  summarise(abundances = sum(value)) %>% 
  ungroup(PlotID, site, treatment, plot, month, time, lifeform) %>%
  spread(lifeform, abundances) %>% 
  replace(is.na(.), 0) %>% 
  remove_rownames %>% 
  dplyr::select(-c(2:6)) %>% 
  column_to_rownames(var="PlotID") %>% 
  hillR::hill_taxa(q = 2, MARGIN = 1) %>% 
  as.data.frame() %>% rownames_to_column(var="PlotID") %>% 
  separate(PlotID, c("Site", "Treatment", "Plot", "Month", "Time"), convert = TRUE) %>% 
  rename(simp.inv.div = ".") %>% 
  hablar::rationalize() %>%
  dplyr::select(c(2,4,5,6)) %>% 
  group_by(Treatment, Time, Month) %>% 
  dplyr::summarize(Mean = mean(simp.inv.div)) %>% 
  ungroup(Treatment, Time, Month)

bt.invsim.mtz  


ggplot(bt.invsim.mtz, aes(x = Month, y = Mean, color = Treatment, group = Treatment)) + 
  geom_line(size = 1) + 
  theme_bw() + 
  labs(y = "Evenness", x = "Months", color = "Life-forms") +
  theme(strip.background = element_rect(color="grey50", fill="gray90"),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.y = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_color_manual(values = c("darkred", "darkgreen"), name = "Treatment", labels = c("Closed", "Open")) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96", "102", "108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/evenness/life_forms_evenness_14-05-20.jpeg", width = 25, height = 15, units = "cm", dpi = 300)


