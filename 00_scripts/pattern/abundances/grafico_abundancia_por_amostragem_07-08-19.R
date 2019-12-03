### YURI - 07-08-19
### Script para mostrar monta o gráfico da abundancia por amostragem e por área

library(tidyverse)
library(beepr)
library(car)
library(broom)


setwd("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/")


ttlplt <- read_csv("abundancia_por_amostragem_07-08-19.csv")
#ttlplt <- ttlplt[-c(107:114),]
ttlplt


### grafico de abundancia total por tratamento, todas as áreas

ggplot(ttlplt, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_boxplot(width = 0.65, alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +
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
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  labs(x = "Time (month)", y = "Seedlings abundance (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                    "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_all_year_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado

ggplot(ttlplt, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) + 
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
  labs(x = "Treatment", y = "Seedlings (All Areas)") +
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  annotate("text", label = "R² = 0.16, P-value = 5.321e-07", size = 5, x = 1.0, y = 850)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_all_treatment_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlplt$Freq)
testet.trat.all <- t.test(ttlplt$Freq~ttlplt$Treatment)
testet.trat.all
testet.trat.all.lm <- lm(ttlplt$Freq~ttlplt$Treatment) 
testet.trat.all.lm
summary(testet.trat.all.lm)




### grafico de abundancia total por tratamento, CARDOSO

ttlpltcar <- ttlplt %>% 
  filter(`Site` == "CAR")
ttlpltcar

ggplot(ttlpltcar, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.0),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  labs(x = "Time (month)", y = "Seedlings abundance (Cardoso Island)") +   scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                    "54","60","66","72","78","84","90","96"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_year_car_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado CAR


ggplot(ttlpltcar, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.2) +
  theme_bw() +
  scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +  
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
  labs(x = "Treatment", y = "Seedlings (Cardoso Island)") +
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  annotate("text", label = "R² = 0.60, P-value = 3.929e-08", size = 5, x = 1.0, y = 850)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_treatment_car_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltcar$Freq)
testet.trat.car <- t.test(ttlpltcar$Freq~ttlpltcar$Treatment)
testet.trat.car
testet.trat.car.lm <- lm(ttlpltcar$Freq~ttlpltcar$Treatment) 
testet.trat.car.lm
summary(testet.trat.car.lm)



### grafico de abundancia total por tratamento, Vargem Grande

ttlpltvgm <- ttlplt %>% 
  filter(`Site` == "VGM")
ttlpltvgm

ggplot(ttlpltvgm, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.0),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  labs(x = "Time (month)", y = "Seedlings abundance (Vargem Grande)") +   scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                     "54","60","66","72","78","84","90","96"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_year_vgm_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado VGM


ggplot(ttlpltvgm, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +  theme_bw() +
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
  labs(x = "Treatment", y = "Seedlings (Vargem Grande)") +
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  annotate("text", label = "R² = 0.88, P-value = 2.2e-16", size = 5, x = 1.0, y = 850)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_treatment_vgm_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltvgm$Freq)
testet.trat.vgm <- t.test(ttlpltvgm$Freq~ttlpltvgm$Treatment)
testet.trat.vgm
testet.trat.vgm.lm <- lm(ttlpltvgm$Freq~ttlpltvgm$Treatment) 
testet.trat.vgm.lm
summary(testet.trat.vgm.lm)



### grafico de abundancia total por tratamento, Carlos Botelho

ttlpltcbo <- ttlplt %>% 
  filter(`Site` == "CBO")
ttlpltcbo

ggplot(ttlpltcbo, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.0),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  labs(x = "Time (month)", y = "Seedlings abundance (Carlos Botelho)") +   scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                    "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_year_cbo_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado CBO


ggplot(ttlpltcbo, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +  theme_bw() +
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
  labs(x = "Treatment", y = "Seedlings (Carlos Botelho)") +
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  annotate("text", label = "R² = -0.02, P-value = 0.64", size = 5, x = 1.0, y = 850)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_treatment_cbo_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltcbo$Freq)
testet.trat.cbo <- t.test(ttlpltcbo$Freq~ttlpltcbo$Treatment)
testet.trat.cbo
testet.trat.cbo.lm <- lm(ttlpltcbo$Freq~ttlpltcbo$Treatment) 
testet.trat.cbo.lm
summary(testet.trat.cbo.lm)



### grafico de abundancia total por tratamento, Itamambuca

ttlpltita <- ttlplt %>% 
  filter(`Site` == "ITA")
ttlpltita

ggplot(ttlpltita, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.0),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.12, .98),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6))  + 
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  labs(x = "Time (month)", y = "Seedlings abundance (Itamambuca)") +   scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                     "54","60","66","72","78","84","90","96"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_year_ita_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)


### Grafico de comparacao total entre aberto fechado ITA


ggplot(ttlpltita, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.2) +
  theme_bw() +   scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) +  theme_bw() +
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
  labs(x = "Treatment", y = "Seedlings (Itamambuca)") +
  expand_limits(y=800) + scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800)) +
  annotate("text", label = "R² = 0.44, P-value = 1.039e-05", size = 5, x = 1.0, y = 850)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/abundancias/abundancia_treatment_ita_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltita$Freq)
testet.trat.ita <- t.test(ttlpltita$Freq~ttlpltita$Treatment)
testet.trat.ita
testet.trat.ita.lm <- lm(ttlpltita$Freq~ttlpltita$Treatment) 
testet.trat.ita.lm
summary(testet.trat.ita.lm)
