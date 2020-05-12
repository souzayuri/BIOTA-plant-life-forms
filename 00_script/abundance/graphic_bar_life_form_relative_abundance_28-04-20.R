# Informations ------------------------------------------------------------

### title: Temporal relative abundance ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 28/04/2020
### Description: this script calculate the plant life-form relative abundance by time


# Load packages and open datatable -------------------------------------
if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("textclean")) install.packages("textclean", dependencies = TRUE)

data.biota <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2020v1_para_abundancia.csv")
# Calculate relative abundances -----------------------------------------

rel.abun <- data.biota %>% 
  rename(life_form = `Life Form`) %>%
  textclean::drop_row("life_form", c("indeterminate", "fern")) %>%
  gather(key = "Month", value = "value", 5:23) %>% 
  mutate(time = Month %>% stringr::str_replace("T", "")) %>% 
  mutate(Treatment = factor(Treatment)) %>% 
  mutate(Treatment = fct_relevel(Treatment, "open", "closed")) %>% 
  mutate(Treatment = recode(Treatment, "open" = "Open", "closed" = "Closed")) %>% 
  mutate(life_form = fct_relevel(life_form, "tree", "palm","liana","shrub","herb","bamboo")) %>%
  mutate(life_form = recode(life_form, "tree" = "Trees", "palm" = "Palms","liana" = "Lianas","shrub" = "Shrubs","herb" = "Herbs","bamboo" = "Bamboos")) %>% 
  dplyr::group_by(Treatment, Month, time, life_form) %>% 
  summarise(abs.abun = sum(value)) %>% 
  group_by(time, Treatment) %>% mutate(rel.abun = abs.abun / sum(abs.abun))
rel.abun 


ggplot(rel.abun, aes(x = time, y = rel.abun, fill = life_form)) + 
  geom_bar(position = position_dodge(width=0.7),
           stat = "identity", colour = "black", width = 1, size = 0.1) + 
  theme_bw() + 
  facet_grid(Treatment~., space="free") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(y = "Relative abundance", x = "Months", fill = "Life-forms") +
  theme(strip.background = element_rect(color="grey50", fill="gray90"),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.y = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_fill_manual(values = c("springgreen4","yellowgreen",
                               "khaki4","tan1","sienna3","brown")) +
  scale_x_discrete(labels = c("01","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96", "102", "108"))


ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/relative_abundance/relative_abun_lf_facetwrap_28-04-20.jpeg", width = 25, height = 15, units = "cm", dpi = 300)

# Calculate relative abundances by density -----------------------------------------

rel.abun <- data.biota %>% 
  rename(life_form = `Life Form`) %>%
  textclean::drop_row("life_form", c("indeterminate")) %>%
  gather(key = "Month", value = "value", 6:22) %>% 
  mutate(time = Month %>% stringr::str_replace("p", "")) %>% 
  dplyr::group_by(Site, Plot, Treatment, Month, time, life_form) %>% 
  summarise(abs.abun = sum(value)) %>% 
  mutate(rel.abun = (abs.abun / sum(abs.abun))/3^2)
rel.abun 



ggplot(rel.abun, aes(x = Month, y = rel.abun, fill = life_form)) + 
  geom_bar(position = "dodge",stat = "identity") + theme_classic() + facet_wrap(~Treatment) +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())
