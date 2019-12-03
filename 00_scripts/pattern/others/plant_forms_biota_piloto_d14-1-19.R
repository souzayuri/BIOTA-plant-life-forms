# graficos projeto piloto parcelas do Biota e habitos

# 12/1/2019
#install.packages("tidyquant")
#install.packages("cranlogs")


library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")
library("tidyquant")  # Loads tidyverse, tidquant, financial pkgs, xts/zoo
library("cranlogs")   # For inspecting package downloads over year


rm(list = ls()) #limpa a memoria

dados_biota <- read_csv("Life_Form_yuri_13-1-19.csv") #carrega a tabela
dados_biota


d.b <- dados_biota %>% 
  gather(key = "year", value = "value", 6:22) #  coleta as colunas de t1:t17 e as agregas na coluna "year"

d.b$Index <- 1:nrow(d.b) # renumera os numeros dos index (pois apos virar a coluna eles ficam repetidos)
d.b

d.b.random <- d.b[sample(1:nrow(d.b), 10),] #faz uma amostragem "sample" aleatorio das linhas. vai da primeira a ultima linha do df "1:nrow(df)" e pega 10 numeros aleatorios
d.b.random # com isso pode comparar com os dados organizados manualmente para uma valida??o

#write.csv(d.b, "BIOTA_plant_life_column_to_row.csv", row.names = FALSE, quote = TRUE) #exporta a nova tabela

### 1) Primeira forma de fazer ####
##  resumidamente: filtra as variaveis que eu vou usar e separa os tratamentos por linha mantendo em unico data.frame

## Lianas
# Ilha do Cardoso
d.b<- read_csv("BIOTA_plant_life_column_to_row_y-m-d.csv") # renomeei o numero p6 para p06, por isso precisa carregar essa tabela. Organizada manualmente por ano-mes-dia
glimpse(d.b)

d.b.car.l <- d.b %>% # carrega como tibble o d.b e guarda no d.b.car.l
  filter(`Site` == "CAR" & `Life_Form` == "liana" & `value` >= 0 ) %>%  # filtra por local, forma de vida e presenca e ausencia
  select(c(1:4, 10:13)) %>% # seleciona apenas as colunas de interesse
  group_by(year, Treatment) %>% # divide por tipos de tratamento
  summarise(abundance = sum(value)) %>% #retira os valores da coluna value, soma eles e guarda numa coluna chamada abundance
  arrange(Treatment) #organiza o tratamento por ordem alfabetica
d.b.car.l

mean(d.b.car.l$abundance)

ggplot(d.b.car.l, aes(year, abundance)) + # carrega variavel x (year) e y (abundance)
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + # define que o grupo é o Treatment (sem isso nao roda) 
  #e diz que as cores serao separadas por ele. Argumentos seguintes e para modificar a transparencia da cor e a sobreposicao das areas preenchidas
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) + # plota os pontos e muda as cores deles
  scale_color_manual(values = c("darkseagreen4", "navajowhite4")) + # modifica as cores dos tratamentos
  scale_fill_manual(values = c("darkseagreen4", "navajowhite4")) + # modifica as cores dos tratamentos
  labs(x = "Time Series",
       y = "Abundance",
       title = "Lianas - Ilha do Cardoso") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/liana_car.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Carlos Botelho
d.b.cbo.l <- d.b %>% 
  filter(`Site` == "CBO" & `Life_Form` == "liana" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.cbo.l

ggplot(d.b.cbo.l, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("lightgreen", "sandybrown")) +
  scale_fill_manual(values = c("lightgreen", "sandybrown")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Lianas - Carlos Botelho") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/liana_cbo.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Itamambuca
d.b.ita.l <- d.b %>% 
  filter(`Site` == "ITA" & `Life_Form` == "liana" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.ita.l

ggplot(d.b.ita.l, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("bisque4", "pink4")) +
  scale_fill_manual(values = c("bisque4", "pink4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Lianas - Itamambuca") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/liana_ita.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Vargem Grande
d.b.vgm.l <- d.b %>% 
  filter(`Site` == "VGM" & `Life_Form` == "liana" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.vgm.l

ggplot(d.b.vgm.l, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("mediumorchid", "deepskyblue4")) +
  scale_fill_manual(values = c("mediumorchid", "deepskyblue4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Lianas - Vargem Grande") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8)) 

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/liana_vgm.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")





  
## Ervas
# Ilha do Cardoso

d.b.car.h <- d.b %>% 
  filter(`Site` == "CAR" & `Life_Form` == "herb" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.car.h

ggplot(d.b.car.h, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("darkseagreen4", "navajowhite4")) +
  scale_fill_manual(values = c("darkseagreen4", "navajowhite4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Herb - Ilha do Cardoso") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/herb_car.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Carlos Botelho
d.b.cbo.h <- d.b %>% 
  filter(`Site` == "CBO" & `Life_Form` == "herb" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.cbo.h

ggplot(d.b.cbo.h, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("lightgreen", "sandybrown")) +
  scale_fill_manual(values = c("lightgreen", "sandybrown")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Herb - Carlos Botelho") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/herb_cbo.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Itamambuca
d.b.ita.h <- d.b %>% 
  filter(`Site` == "ITA" & `Life_Form` == "herb" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.ita.h

ggplot(d.b.ita.h, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("bisque4", "pink4")) +
  scale_fill_manual(values = c("bisque4", "pink4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Herb - Itamambuca") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/herb_ita.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Vargem Grande
d.b.vgm.h <- d.b %>% 
  filter(`Site` == "VGM" & `Life_Form` == "herb" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.vgm.h

ggplot(d.b.vgm.h, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("mediumorchid", "deepskyblue4")) +
  scale_fill_manual(values = c("mediumorchid", "deepskyblue4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Herb - Vargem Grande") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8)) 

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/herb_vgm.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")





## Bamboo
# Ilha do Cardoso

d.b.car.b <- d.b %>% 
  filter(`Site` == "CAR" & `Life_Form` == "bamboo" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.car.b

ggplot(d.b.car.b, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("darkseagreen4", "navajowhite4")) +
  scale_fill_manual(values = c("darkseagreen4", "navajowhite4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Bamboo - Ilha do Cardoso") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/bamboo_car.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Carlos Botelho
d.b.cbo.b <- d.b %>% 
  filter(`Site` == "CBO" & `Life_Form` == "bamboo" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.cbo.b

ggplot(d.b.cbo.b, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("lightgreen", "sandybrown")) +
  scale_fill_manual(values = c("lightgreen", "sandybrown")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Bamboo - Carlos Botelho") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/bamboo_cbo.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")


# Itamambuca
d.b.ita.b <- d.b %>% 
  filter(`Site` == "ITA" & `Life_Form` == "bamboo" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.ita.b

ggplot(d.b.ita.b, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("bisque4", "pink4")) +
  scale_fill_manual(values = c("bisque4", "pink4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Bamboo - Itamambuca") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/bamboo_ita.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")


# Vargem Grande
d.b.vgm.b <- d.b %>% 
  filter(`Site` == "VGM" & `Life_Form` == "bamboo" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.vgm.b

ggplot(d.b.vgm.b, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("mediumorchid", "deepskyblue4")) +
  scale_fill_manual(values = c("mediumorchid", "deepskyblue4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Bamboo - Vargem Grande") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8)) 

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/bamboo_vgm.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")






## Tree
# Ilha do Cardoso

d.b.car.t <- d.b %>% 
  filter(`Site` == "CAR" & `Life_Form` == "tree" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.car.t

ggplot(d.b.car.t, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("darkseagreen4", "navajowhite4")) +
  scale_fill_manual(values = c("darkseagreen4", "navajowhite4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Tree - Ilha do Cardoso") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/tree_car.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")


# Carlos Botelho
d.b.cbo.t <- d.b %>% 
  filter(`Site` == "CBO" & `Life_Form` == "tree" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.cbo.t

ggplot(d.b.cbo.t, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("lightgreen", "sandybrown")) +
  scale_fill_manual(values = c("lightgreen", "sandybrown")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Tree - Carlos Botelho") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/tree_cbo.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Itamambuca
d.b.ita.t <- d.b %>% 
  filter(`Site` == "ITA" & `Life_Form` == "tree" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.ita.t

ggplot(d.b.ita.t, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("bisque4", "pink4")) +
  scale_fill_manual(values = c("bisque4", "pink4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Tree - Itamambuca") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/tree_ita.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Vargem Grande
d.b.vgm.t <- d.b %>% 
  filter(`Site` == "VGM" & `Life_Form` == "tree" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.vgm.t

ggplot(d.b.vgm.t, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("mediumorchid", "deepskyblue4")) +
  scale_fill_manual(values = c("mediumorchid", "deepskyblue4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Tree - Vargem Grande") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8)) 

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/tree_vgm.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")





## Palmeira
# Ilha do Cardoso

d.b.car.p <- d.b %>% 
  filter(`Site` == "CAR" & `Life_Form` == "palm" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.car.p

ggplot(d.b.car.p, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("darkseagreen4", "navajowhite4")) +
  scale_fill_manual(values = c("darkseagreen4", "navajowhite4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Palm - Ilha do Cardoso") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/palm_car.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Carlos Botelho
d.b.cbo.p <- d.b %>% 
  filter(`Site` == "CBO" & `Life_Form` == "palm" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.cbo.p

ggplot(d.b.cbo.p, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("lightgreen", "sandybrown")) +
  scale_fill_manual(values = c("lightgreen", "sandybrown")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Palm - Carlos Botelho") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/palm_cbo.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Itamambuca
d.b.ita.p <- d.b %>% 
  filter(`Site` == "ITA" & `Life_Form` == "palm" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.ita.p

ggplot(d.b.ita.p, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("bisque4", "pink4")) +
  scale_fill_manual(values = c("bisque4", "pink4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Palm - Itamambuca") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8))

#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/palm_ita.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

# Vargem Grande
d.b.vgm.p <- d.b %>% 
  filter(`Site` == "VGM" & `Life_Form` == "palm" & `value` >= 0 ) %>% 
  select(c(1:4, 10:12)) %>% 
  group_by(year, Treatment) %>%
  summarise(abundance = sum(value)) %>% 
  arrange(Treatment)
d.b.vgm.p

ggplot(d.b.vgm.p, aes(year, abundance)) +
  theme_light() +
  geom_area(aes(group = Treatment, color = Treatment, fill = Treatment), alpha = 0.55, position = position_dodge(0.8)) + 
  geom_point(colour = "black", fill = "seagreen4", size = 2, alpha = .65, pch = 21) +
  scale_color_manual(values = c("mediumorchid", "deepskyblue4")) +
  scale_fill_manual(values = c("mediumorchid", "deepskyblue4")) +
  labs(x = "Time Series",
       y = "Abundance",
       title = "Palm - Vargem Grande") +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=8)) 


#tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/analises/02_figuras/palm_vgm.tif", wi = 15, he = 15, un = "cm", res = 300, comp = "lzw+p")

## 2) Segunda forma de fazer
##  resumidamente: filtra as variaveis que eu vou usar e separa os tratamentos 
#e depois junta em um unico data.frame com as colunas separadas por tratamento
#rm(list = ls())


#d.b <- as.data.frame(read_csv("planilha_biota_period_ind__only_species_R_13-1-19.csv"))
#str(d.b)

#d.b.l.c.o <- d.b %>% 
#  filter(`Site` == "CAR" & `Life_Form` == "liana" & `value` == 1 & `Treatment` == "open") %>% 
#  select(c(1:4, 10:12)) %>% 
#  count(year)
#d.b.l.c.o <- rename(d.b.l.c.o, c(n = "species_number_car_o"))
#d.b.l.c.o


#d.b.l.c.c <- d.b %>% 
#  filter(`Site` == "CAR" & `Life_Form` == "liana" & `value` == 1 & `Treatment` == "close") %>% 
#  select(c(1:4, 10:12)) %>% 
#  count(year)
#d.b.l.c.c <- rename(d.b.l.c.c, c(n = "species_number_car_c"))
#d.b.l.c.c



#d.b.l.car <- d.b.l.c.c %>% 
#  left_join(d.b.l.c.o, by = "year")  
#d.b.l.car




warning()
