### title: Boxplot do total de plantulas por grupo ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 09/28/2019
### Description: Abundancia de grupos em cada parcela e em cada amostragem por tratamento

rm(list = ls()) 

library(tidyverse)

bt.dt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
bt.dt 

bt.dt.slc <- bt.dt [,-c(1,5,23:28)]
bt.dt.slc

bt.dt.slc.gtr <- bt.dt.slc %>% 
  gather(key = "time", value = "value", 4:20)
bt.dt.slc.gtr

### Palmeiras

palmeiras <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "palm" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value)))
palmeiras


palmeiras_vln <- ggplot(data = palmeiras, 
                        aes(x = time, y = abundancia, fill = Treatment)) + 
  geom_violin(aes(fill = Treatment), trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.7) + 
  geom_boxplot(aes(group = interaction(Treatment, time)),color = "black", width = 0.3, fill = "gray", outlier.colour= "black", position = position_dodge(width = 0.9), alpha = 0.7) + 
  scale_fill_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.8, linetype = "solid"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_line(color = "gray", size = 0.5)) +
  labs(x = "Sampled period (months)", y = "Total palms abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))

palmeiras_vln

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/grafico_violino_e_boxplot/abundance_log_palms.png", w = 25, h = 10, unit = "cm", dpi = 300)


### Arvores

arvores <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "tree" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value)))
arvores


arvores_vln <- ggplot(data = arvores, 
                        aes(x = time, y = abundancia, fill = Treatment)) + 
  geom_violin(aes(fill = Treatment), trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.7) + 
  geom_boxplot(aes(group = interaction(Treatment, time)),color = "black", width = 0.3, fill = "gray", outlier.colour= "black", position = position_dodge(width = 0.9), alpha = 0.7) + 
  scale_fill_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.8, linetype = "solid"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_line(color = "gray", size = 0.5)) +
  labs(x = "Sampled period (months)", y = "Total trees abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))

arvores_vln

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/grafico_violino_e_boxplot/abundance_log_trees.png", w = 25, h = 10, unit = "cm", dpi = 300)


### Ervas

ervas <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "herb" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value)))
ervas


ervas_vln <- ggplot(data = ervas, 
                      aes(x = time, y = abundancia, fill = Treatment)) + 
  geom_violin(aes(fill = Treatment), trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.7) + 
  geom_boxplot(aes(group = interaction(Treatment, time)),color = "black", width = 0.3, fill = "gray", outlier.colour= "black", position = position_dodge(width = 0.9), alpha = 0.7) + 
  scale_fill_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.8, linetype = "solid"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_line(color = "gray", size = 0.5)) +
  labs(x = "Sampled period (months)", y = "Total herbs abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))

ervas_vln

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/grafico_violino_e_boxplot/abundance_log_herbs.png", w = 25, h = 10, unit = "cm", dpi = 300)


### Bamboos

bamboos <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "bamboo" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value)))
bamboos


bamboos_vln <- ggplot(data = bamboos, 
                    aes(x = time, y = abundancia, fill = Treatment)) + 
  geom_violin(aes(fill = Treatment), trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.7) + 
  geom_boxplot(aes(group = interaction(Treatment, time)),color = "black", width = 0.3, fill = "gray", outlier.colour= "black", position = position_dodge(width = 0.9), alpha = 0.7) + 
  scale_fill_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.8, linetype = "solid"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_line(color = "gray", size = 0.5)) +
  labs(x = "Sampled period (months)", y = "Total bamboos abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))

bamboos_vln

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/grafico_violino_e_boxplot/abundance_log_bamboos.png", w = 25, h = 10, unit = "cm", dpi = 300)



### Lianas

lianas <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "liana" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value)))
lianas


lianas_vln <- ggplot(data = lianas, 
                      aes(x = time, y = abundancia, fill = Treatment)) + 
  geom_violin(aes(fill = Treatment), trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.7) + 
  geom_boxplot(aes(group = interaction(Treatment, time)),color = "black", width = 0.2, fill = "gray", outlier.colour= "black", position = position_dodge(width = 0.9), alpha = 0.7) + 
  scale_fill_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.8, linetype = "solid"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_line(color = "gray", size = 0.5)) +
  labs(x = "Sampled period (months)", y = "Total lianas abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))

lianas_vln

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/grafico_violino_e_boxplot/abundance_log_lianas.png", w = 25, h = 10, unit = "cm", dpi = 300)


### Arbusto

arbustos <- bt.dt.slc.gtr %>% 
  filter(`Life Form` == "shrub" & `value` == "1") %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = log(sum(value)))
arbustos


arbustos_vln <- ggplot(data = arbustos, 
                     aes(x = time, y = abundancia, fill = Treatment)) + 
  geom_violin(aes(fill = Treatment), trim = FALSE, position = position_dodge(width = 0.9), alpha = 0.7) + 
  geom_boxplot(aes(group = interaction(Treatment, time)),color = "black", width = 0.3, fill = "gray", outlier.colour= "black", position = position_dodge(width = 0.9), alpha = 0.7) + 
  scale_fill_brewer(palette = "Dark2", name = "Treatment", labels = c("Closed", "Open")) + theme_classic() +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(color = "black", size = 0.8, linetype = "solid"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_line(color = "gray", size = 0.5)) +
  labs(x = "Sampled period (months)", y = "Total shrubs abundances (log)") +
  scale_x_discrete(labels = c("0","06","12","18","24","30","36","42","48","54","60","66","72","78","84","90","96"))

arbustos_vln

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/grupos/grafico_violino_e_boxplot/abundance_log_shrubs.png", w = 25, h = 10, unit = "cm", dpi = 300)




### grafico de curva de densidade


library(viridis)
library(ggridges)
library(tidyverse)

comun.group <- rbind(arvores,palmeiras,bamboos,ervas,lianas,arbustos) %>% 
  rename(life_form = `Life Form`)
comun.group



ggplot(comun.group, aes(x = time , y = life_form, fill = abundancia)) + 
  geom_density_ridges_gradient(scale = 0.9, gradient_lwd = 0.5, 
                               color = "black") + 
  scale_fill_viridis(option = "plasma", name = "") + 
  labs(x = "time", y = "Plant Life") +
  theme_ridges(font_family = "Roboto Condensed", grid = F)
