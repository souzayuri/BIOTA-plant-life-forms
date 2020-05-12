library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")
library("tidyquant")  # Loads tidyverse, tidquant, financial pkgs, xts/zoo
library("cranlogs")   # For inspecting package downloads over year
library("survival")


### Cardoso

piloto <- as.data.frame(read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv"))

str(piloto)

CAR <- piloto  %>% 
  filter( Site == "VGM" & `Life_Form` == "liana" & `value` == 1 ) %>%  
  select(c(1:4, 10:13)) %>% 
  group_by(Plot, Treatment, time, value) %>% 
  summarise(abundance = sum(value))


ggplot(CAR, aes(x=Plot, y=abundance, color = Treatment)) +  #group=interaction(treatment, replicate)))
  geom_line() + geom_point() + facet_wrap(~time, scales="free_y") +
  scale_color_manual(values=c("red", "black")) + theme_bw() + 
  labs(x = "Parcelas",
       y = "Abundância",
       title = "") +
  theme(axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ajuste <- lm(CAR$abundance~CAR$Treatment)
ajuste
summary(ajuste)
anova(ajuste)
  