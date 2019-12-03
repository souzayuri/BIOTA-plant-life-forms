### Yuri - 04-07-2019
### Script para calcular a proporcao de especies por area e por forma de vida


library(tidyverse)
library(dplyr)
library(plyr)
library(textclean)
library(wesanderson)
library(beepr)


rm(list = ls())

ttlplt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
ttlplt[,23:24] <- lapply(ttlplt[,23:24], as.numeric) #transforma as colunas 23 e 24 em numerico
ttlplt


#### Para todas as especies

ttlpltlf <- table(ttlplt$Species,ttlplt$`Life Form`) #converse a coluna de forma de vida em uma matriz, 
#junto com as espécies, mostrando qual espécies pertence a quais grupos de vida
ttlpltlf 

### precisa fazer esse processo abaixo para poder converter a tabela a cima em uma formato lido pelo tidyverse (tibble)
write.csv(ttlpltlf , "teste_7-4-19.csv", row.names = TRUE, quote = TRUE) # Salva em uma tabela
ttlpltlf  <- read_csv("teste_7-4-19.csv") # carrega essa tabela. 
ttlpltlf
unlink("teste_7-4-19.csv") # deleta a tabela da pasta


listaspcc <- ttlpltlf  %>% 
  gather(key = "lifeform", value = "freq", 2:8) #coloca todas as colunas da matriz em uma unica
listaspcc
names(listaspcc)[1] <- "Species" # da o cabeçario de Species para a coluna X1

listaspcclf <- listaspcc %>%
  dplyr::filter(!grepl("cf.$", Species)) %>% # remover os cf. que estejam no final do nome
  textclean::drop_row("Species", c("sp.","Rubiaceae", "Myrtaceae")) %>% # remove os 3 elementos listados
  filter(`freq` > 0) %>% # remove as espécies que foram preenchidas por outro grupo funcional 
                        # e mantém só aquelas que estão representadas pelo grupo funcional correto
  mutate(prop = ((freq*100))/sum(freq)) %>% #faz um calculo de proporção de individuos por spp e salva em uma coluna
  mutate(bolean = prop >= 1) %>% # preenche uma coluna com oque está acima de 1% de contribuição
  arrange(prop) %>% # organizada em ordem
  textclean::drop_row("bolean", "FALSE") # remove tudo oque for FALSE da coluna bolean
listaspcclf

colunas <- c("Species", "lifeform", "freq", "prop", "bolean")
todaspp <- data.frame("Others 151 sp. < 1% ...", "Mixed", 1, 1.0, FALSE) 
# coloco os valores 1 e 1.0 para ficar localizado no final do gráfico, 
#mas os valos corretos seriam 957 individuos representando juntos 22.35%
names(todaspp) <- colunas
listaspcclf <- rbind(listaspcclf,todaspp)
listaspcclf

sum(listaspcclf$freq)

ggplot(data = listaspcclf, 
       aes(x = fct_reorder(Species, prop, .desc = TRUE),
           y = prop, fill = bolean)) +  theme_bw() +
  theme(legend.position="none") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=wes_palette("Cavalcanti1")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=20)) + ylab("Proportion (%)") + xlab("Species") +
  expand_limits(y=40) + scale_y_continuous(breaks=c(0,10,20,30,40))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_lifeform_total_euterpe_cf.JPEG", he = 20, wi = 40, un = "cm", dpi = 300)



#### Para CAR

ttlpltcar <- ttlplt %>% 
  filter(`Site` == "CAR")
ttlpltlf <- table(ttlpltcar$Species,ttlpltcar$`Life Form`)
ttlpltlf
write.csv(ttlpltlf , "teste_7-4-19_car.csv", row.names = TRUE, quote = TRUE)
ttlpltlfcar  <- read_csv("teste_7-4-19_car.csv")
unlink("teste_7-4-19_car.csv")

ttlpltlfcar 

listaspccCAR <- ttlpltlfcar  %>% 
  gather(key = "lifeform", value = "freq", 2:7)
listaspccCAR
names(listaspccCAR)[1] <- "Species"

listaspcclfCAR <- listaspccCAR %>%
  dplyr::filter(!grepl("cf.$", Species)) %>% 
  filter_row("Species", c("sp.", "Myrtaceae", "Rubiaceae")) %>% 
  filter(`freq` > 0) %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
listaspcclfCAR

sum(listaspcclfCAR$prop)

colunas <- c("Species", "lifeform", "freq", "prop", "bolean")
todasppCAR <- data.frame("Others 52 sp. < 1% ...", "Mixed", 1, 1.0, TRUE) 
# coloco os valores 1 e 1.0 para ficar localizado no final do gráfico, 
#mas os valos corretos seriam 166 individuos representando juntos 12.82%
names(todasppCAR) <- colunas
listaspcclfCAR <- rbind(listaspcclfCAR,todasppCAR)
listaspcclfCAR

ggplot(data = listaspcclfCAR, 
       aes(x = fct_reorder(Species, prop, .desc = TRUE),
           y = prop, fill = lifeform)) +  theme_bw() +
  theme(legend.position="none") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("gold2", "aquamarine2","royalblue4", "cyan3")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=20)) + ylab("") + xlab("") +
  #theme(plot.background = element_rect(fill = "gray")) +
  expand_limits(y=70) + scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_lifeform_total_CAR_euterpe_cf.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)



#### Para CBO

ttlpltcbo <- ttlplt %>% 
  filter(`Site` == "CBO")
ttlpltlf <- table(ttlpltcbo$Species,ttlpltcbo$`Life Form`)
ttlpltlf
write.csv(ttlpltlf , "teste_7-4-19_cbo.csv", row.names = TRUE, quote = TRUE)
ttlpltlfcbo  <- read_csv("teste_7-4-19_cbo.csv")
unlink("teste_7-4-19_cbo.csv")

listaspccCBO <- ttlpltlfcbo  %>% 
  gather(key = "lifeform", value = "freq", 2:8)
listaspccCBO
names(listaspccCBO)[1] <- "Species"

listaspcclfCBO <- listaspccCBO %>%
  dplyr::filter(!grepl("cf.$", Species)) %>% 
  filter_row("Species", c("sp.", "Myrtaceae", "Rubiaceae")) %>% 
  filter(`freq` > 0) %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
listaspcclfCBO

sum(listaspcclfCBO$freq)

colunas <- c("Species", "lifeform", "freq", "prop", "bolean")
todasppCBO <- data.frame("Others 59 sp. < 1% ...", "Mixed", 1, 1.0, TRUE) 
# coloco os valores 1 e 1.0 para ficar localizado no final do gráfico, 
#mas os valos corretos seriam 144 individuos representando juntos 20.12%
names(todasppCBO) <- colunas
listaspcclfCBO <- rbind(listaspcclfCBO,todasppCBO)
listaspcclfCBO

ggplot(data = listaspcclfCBO, 
       aes(x = fct_reorder(Species, prop, .desc = TRUE),
           y = prop, fill = lifeform)) +  theme_bw() +
  theme(legend.position="") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("chartreuse4","cornflowerblue","gold2","aquamarine2", "royalblue4", "cyan3")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=20)) + ylab("") + xlab("") +
  expand_limits(y=70) + scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_lifeform_total_CBO_euterpe_cf.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)




#### Para VGM

ttlpltvgm <- ttlplt %>% 
  filter(`Site` == "VGM")
ttlpltlf <- table(ttlpltvgm$Species,ttlpltvgm$`Life Form`)
ttlpltlf
write.csv(ttlpltlf , "teste_7-4-19_vgm.csv", row.names = TRUE, quote = TRUE)
ttlpltlfvgm  <- read_csv("teste_7-4-19_vgm.csv")
unlink("teste_7-4-19_vgm.csv")

listaspccVGM <- ttlpltlfvgm  %>% 
  gather(key = "lifeform", value = "freq", 2:8)
listaspccVGM
names(listaspccVGM)[1] <- "Species"

listaspcclfVGM <- listaspccVGM %>%
  dplyr::filter(!grepl("cf.$", Species)) %>% 
  filter_row("Species", c("sp.", "Myrtaceae", "Rubiaceae")) %>% 
  filter(`freq` > 0) %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
listaspcclfVGM

sum(listaspcclfVGM$prop)

colunas <- c("Species", "lifeform", "freq", "prop", "bolean")
todasppVGM <- data.frame("Others 52 sp. < 1% ...", "Mixed", 1, 1.0, TRUE) 
# coloco os valores 1 e 1.0 para ficar localizado no final do gráfico, 
#mas os valos corretos seriam 145 individuos representando juntos 11.21%
names(todasppVGM) <- colunas
listaspcclfVGM <- rbind(listaspcclfVGM,todasppVGM)
listaspcclfVGM

ggplot(data = listaspcclfVGM, 
       aes(x = fct_reorder(Species, prop, .desc = TRUE),
           y = prop, fill = lifeform)) +  theme_bw() +
  theme(legend.position="") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("red4","royalblue4","gold2","aquamarine2","chartreuse4","cyan3")) + 
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=20)) + ylab("") + xlab("") +
  expand_limits(y=70) + scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_lifeform_total_VGM_euterpe_cf.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)




#### Para ITA

ttlpltita <- ttlplt %>% 
  filter(`Site` == "ITA")
ttlpltlf <- table(ttlpltita$Species,ttlpltita$`Life Form`)
ttlpltlf
write.csv(ttlpltlf , "teste_7-4-19_ita.csv", row.names = TRUE, quote = TRUE)
ttlpltlfita  <- read_csv("teste_7-4-19_ita.csv")
unlink("teste_7-4-19_ita.csv")

listaspccITA <- ttlpltlfita  %>% 
  gather(key = "lifeform", value = "freq", 2:8)
listaspccITA
names(listaspccITA)[1] <- "Species"

listaspcclfITA <- listaspccITA %>%
  dplyr::filter(!grepl("cf.$", Species)) %>% 
  filter_row("Species", c("sp.", "Myrtaceae", "Rubiaceae")) %>% 
  filter(`freq` > 0) %>% 
  mutate(prop = ((freq*100))/sum(freq)) %>% 
  mutate(bolean = prop >= 1) %>% 
  arrange(prop) %>% 
  filter_row("bolean", "FALSE")
listaspcclfITA

sum(listaspcclfITA$freq)

colunas <- c("Species", "lifeform", "freq", "prop", "bolean")
todasppITA <- data.frame("Others 41 sp. < 1% ...", "Mixed", 1, 1.0, TRUE) 
# coloco os valores 1 e 1.0 para ficar localizado no final do gráfico, 
#mas os valos corretos seriam 117 individuos representando juntos 11.97%
names(todasppITA) <- colunas
listaspcclfITA <- rbind(listaspcclfITA,todasppITA)
listaspcclfITA

ggplot(data = listaspcclfITA, 
       aes(x = fct_reorder(Species, prop, .desc = TRUE),
           y = prop, fill = lifeform)) +  theme_bw() +
  theme(legend.position="") +
  geom_bar(stat = 'identity', colour="black", width = 0.55) +
  scale_fill_manual(values=c("red4","chartreuse4","cornflowerblue","gold2","aquamarine2","royalblue4","cyan3")) + # usar para legenda scale_fill_manual(name = "", labels = c("Bamboos", "Herbs", "Lianas", "Palm", "Shrubs", "Trees")
  theme(axis.text.x = element_text(angle=60, hjust=1, colour="black", size=rel(0.6))) + 
  theme(text = element_text(size=20)) + ylab("") + xlab("") +
  expand_limits(y=70) + scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/proporcao/especies_lifeform_total_ITA_euterpe_cf.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)
