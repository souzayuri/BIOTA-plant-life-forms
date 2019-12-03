### YURI - 25-08-19
### Script para mostrar monta o gráfico da abundancia por amostragem por grupos funcionais e área

library(tidyverse)
library(beepr)
library(car)
library(broom)


setwd("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/abundancia")


### Herbs ###

ttlplt <- read_csv("abundancia_por_amostragem_herb_25-08-19.csv")
#ttlplt <- ttlplt[-c(107:114),]
ttlplt


### grafico de abundancia total por tratamento, todas as áreas - Herbs

ggplot(ttlplt, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_boxplot(width = 0.65, alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  labs(x = "Time (month)", y = "Herb's abundance (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                              "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_year_herb_25-08-19.PNG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado - Herbs

ggplot(ttlplt, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.98, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_discrete(labels=c("Closed", "Open")) +
  labs(x = "Treatment", y = "Herb's abundance (All Areas)") +
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  annotate("text", label = "R² = 0.05, P-value = 0.002", size = 5, x = 1.0, y = 350)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_treatment_herbs_25-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)


### Anova e test-t para todas as áreas - Herbs
shapiro.test(ttlplt$Freq)
testet.trat.all_herb <- t.test(ttlplt$Freq~ttlplt$Treatment)
testet.trat.all_herb
testet.trat.all.lm_herb <- lm(ttlplt$Freq~ttlplt$Treatment) 
testet.trat.all.lm_herb
summary(testet.trat.all.lm_herb)




### Liana ###

ttlplt <- read_csv("abundancia_por_amostragem_liana_25-08-19.csv")
#ttlplt <- ttlplt[-c(107:114),]
ttlplt


### grafico de abundancia total por tratamento, todas as áreas - Liana

ggplot(ttlplt, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_boxplot(width = 0.65, alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  labs(x = "Time (month)", y = "Liana's abundance (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                          "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_year_lianas_25-08-19.PNG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado - Lianas

ggplot(ttlplt, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.98, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_discrete(labels=c("Closed", "Open")) +
  labs(x = "Treatment", y = "Liana's abundance (All Areas)") +
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  annotate("text", label = "R² = 0.11, P-value = 2.598e-05", size = 5, x = 1.0, y = 350)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_treatment_lianas_25-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)


### Anova e test-t para todas as áreas - Liana
shapiro.test(ttlplt$Freq)
testet.trat.all_liana <- t.test(ttlplt$Freq~ttlplt$Treatment)
testet.trat.all_liana
testet.trat.all.lm_liana <- lm(ttlplt$Freq~ttlplt$Treatment) 
testet.trat.all.lm_liana
summary(testet.trat.all.lm_liana)




### Trees ###

ttlplt <- read_csv("abundancia_por_amostragem_trees_25-08-19.csv")
#ttlplt <- ttlplt[-c(107:114),]
ttlplt


### grafico de abundancia total por tratamento, todas as áreas - Trees

ggplot(ttlplt, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_boxplot(width = 0.65, alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  labs(x = "Time (month)", y = "Tree's abundance (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                           "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_year_trees_25-08-19.PNG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado - Trees

ggplot(ttlplt, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.98, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_discrete(labels=c("Closed", "Open")) +
  labs(x = "Treatment", y = "Tree's abundance (All Areas)") +
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  annotate("text", label = "R² = 0.22, P-value = 2.954e-09", size = 5, x = 1.0, y = 350)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_treatment_trees_25-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)


### Anova e test-t para todas as áreas - Trees
shapiro.test(ttlplt$Freq)
testet.trat.all_tree <- t.test(ttlplt$Freq~ttlplt$Treatment)
testet.trat.all_tree
testet.trat.all.lm_tree <- lm(ttlplt$Freq~ttlplt$Treatment) 
testet.trat.all.lm_tree
summary(testet.trat.all.lm_tree)




### Shrubs ###

ttlplt <- read_csv("abundancia_por_amostragem_shrub_25-08-19.csv")
#ttlplt <- ttlplt[-c(107:114),]
ttlplt


### grafico de abundancia total por tratamento, todas as áreas - Shrub

ggplot(ttlplt, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_boxplot(width = 0.65, alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  labs(x = "Time (month)", y = "Shrub's abundance (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                           "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_year_shrub_25-08-19.PNG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado - Shrub

ggplot(ttlplt, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.98, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_discrete(labels=c("Closed", "Open")) +
  labs(x = "Treatment", y = "Shrub's abundance (All Areas)") +
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  annotate("text", label = "R² = 0.04, P-value = 0.008", size = 5, x = 1.0, y = 350)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_treatment_shrub_25-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)


### Anova e test-t para todas as áreas - Shrub
shapiro.test(ttlplt$Freq)
testet.trat.all_shrub <- t.test(ttlplt$Freq~ttlplt$Treatment)
testet.trat.all_shrub
testet.trat.all.lm_shrub <- lm(ttlplt$Freq~ttlplt$Treatment) 
testet.trat.all.lm_shrub
summary(testet.trat.all.lm_shrub)




### Palm ###

ttlplt <- read_csv("abundancia_por_amostragem_palm_25-08-19.csv")
#ttlplt <- ttlplt[-c(107:114),]
ttlplt


### grafico de abundancia total por tratamento, todas as áreas - Palm

ggplot(ttlplt, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_boxplot(width = 0.65, alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  labs(x = "Time (month)", y = "Palm's abundance (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                            "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_year_palm_25-08-19.PNG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado - Palm

ggplot(ttlplt, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.98, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_discrete(labels=c("Closed", "Open")) +
  labs(x = "Treatment", y = "Palm's abundance (All Areas)") +
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  annotate("text", label = "R² = 0.08, P-value = 0.0003", size = 5, x = 1.0, y = 350)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_treatment_palm_25-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)


### Anova e test-t para todas as áreas - Palm
shapiro.test(ttlplt$Freq)
testet.trat.all_palm <- t.test(ttlplt$Freq~ttlplt$Treatment)
testet.trat.all_palm
testet.trat.all.lm_palm <- lm(ttlplt$Freq~ttlplt$Treatment) 
testet.trat.all.lm_palm
summary(testet.trat.all.lm_palm)




### Bamboo ###

ttlplt <- read_csv("abundancia_por_amostragem_bamboo_25-08-19.csv")
#ttlplt <- ttlplt[-c(107:114),]
ttlplt


### grafico de abundancia total por tratamento, todas as áreas - Bamboos

ggplot(ttlplt, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_boxplot(width = 0.65, alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  labs(x = "Time (month)", y = "Bamboo's abundance (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                           "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_year_bamboos_25-08-19.PNG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado - Bamboos

ggplot(ttlplt, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("darkorange4","forestgreen")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.98, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) +
  scale_x_discrete(labels=c("Closed", "Open")) +
  labs(x = "Treatment", y = "Bamboo's abundance (All Areas)") +
  expand_limits(y=350) + scale_y_continuous(breaks=c(50,100,150,200,250,300)) +
  annotate("text", label = "R² = 0.01, P-value = 0.17", size = 5, x = 1.0, y = 350)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/functional_groups/abundancia_all_treatment_bamboos_25-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)


### Anova e test-t para todas as áreas - bamboo
shapiro.test(ttlplt$Freq)
testet.trat.all_bamboo <- t.test(ttlplt$Freq~ttlplt$Treatment)
testet.trat.all_bamboo
testet.trat.all.lm_bamboo <- lm(ttlplt$Freq~ttlplt$Treatment) 
testet.trat.all.lm_bamboo
summary(testet.trat.all.lm_bamboo)

beep(5)
