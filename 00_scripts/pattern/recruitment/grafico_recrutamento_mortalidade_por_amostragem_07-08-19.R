### YURI - 27-6-19
### Script para grafico de recrutamento de individuos que continuam vivos após a primeira amostragem

library(tidyverse)
library(beepr)

setwd("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/recrutamento/")


ttlpltrm <- read_csv("recrut_mort_por_amostragem_07-08-19.csv")
ttlpltrm


### grafico de recrutamento por tratamento, todas as áreas

ggplot(ttlpltrm, aes(x=Period, y=Freq, fill = Treatment)) + 
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
        legend.position = c(0.88, .98),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) + 
  expand_limits(y=100) + scale_y_continuous(breaks=c(0,50,100)) +
  labs(x = "Time (month)", y = "Recruitment before mortality (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                          "54","60","66","72","78","84","90","96"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_year_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado

ggplot(ttlpltrm, aes(x=Treatment, y=Freq, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) + geom_jitter(width = 0.2) +
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
  labs(x = "Treatment", y = "Recruitment before mortality (All Areas)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = -0.001, P-value = 0.3715", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_treatm_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrm$Freq)
testet.trat.all.rm <- t.test(ttlpltrm$Freq~ttlpltrm$Treatment)
testet.trat.all.rm
testet.trat.all.lm.rm <- lm(ttlpltrm$Freq~ttlpltrm$Treatment) 
testet.trat.all.lm.rm
summary(testet.trat.all.lm.rm)



### grafico de recrutamento por tratamento, CARDOSO

ttlpltrmcar <- ttlpltrm %>% 
  filter(`Site` == "CAR")
ttlpltrmcar


ggplot(ttlpltrmcar, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.88, .98),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) + 
  expand_limits(y=100) + scale_y_continuous(breaks=c(0,50,100)) +
  labs(x = "Time (month)", y = "Recruitment before mortality (Cardoso Island)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                       "54","60","66","72","78","84"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_year_car_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado CAR

ggplot(ttlpltrmcar, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment before mortality (Cardoso Island)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = -0.01, P-value = 0.4693", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_treatm_car_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrmcar$Freq)
testet.trat.all.rm.car <- t.test(ttlpltrmcar$Freq~ttlpltrmcar$Treatment)
testet.trat.all.rm.car
testet.trat.all.lm.rm.car <- lm(ttlpltrmcar$Freq~ttlpltrmcar$Treatment) 
testet.trat.all.lm.rm.car
summary(testet.trat.all.lm.rm.car)





### grafico de recrutamento por tratamento, Vargem Grande

ttlpltrmvgm <- ttlpltrm %>% 
  filter(`Site` == "VGM")
ttlpltrmvgm


ggplot(ttlpltrmvgm, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.88, .98),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) + 
  expand_limits(y=100) + scale_y_continuous(breaks=c(0,50,100)) +
  labs(x = "Time (month)", y = "Recruitment before mortality (Vargem Grande)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                            "54","60","66","72","78","84"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_year_vgm_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado VGM

ggplot(ttlpltrmvgm, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment before mortality (Vargem Grande)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = -0.02, P-value = 0.5305", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_treatm_vgm_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrmvgm$Freq)
testet.trat.all.rm.vgm <- t.test(ttlpltrmvgm$Freq~ttlpltrmvgm$Treatment)
testet.trat.all.rm.vgm
testet.trat.all.lm.rm.vgm <- lm(ttlpltrmvgm$Freq~ttlpltrmvgm$Treatment) 
testet.trat.all.lm.rm.vgm
summary(testet.trat.all.lm.rm.vgm)





### grafico de recrutamento por tratamento, Carlos Botelho

ttlpltrmcbo <- ttlpltrm %>% 
  filter(`Site` == "CBO")
ttlpltrmcbo


ggplot(ttlpltrmcbo, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.88, .98),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) + 
  expand_limits(y=100) + scale_y_continuous(breaks=c(0,50,100)) +
  labs(x = "Time (month)", y = "Recruitment before mortality (Carlos Botelho)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                           "54","60","66","72","78","84","90","96","102"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_year_cbo_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado CBO

ggplot(ttlpltrmcbo, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment before mortality (Carlos Botelho)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = -0.03, P-value = 0.8216", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_treatm_cbo_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrmcbo$Freq)
testet.trat.all.rm.cbo <- t.test(ttlpltrmcbo$Freq~ttlpltrmcbo$Treatment)
testet.trat.all.rm.cbo
testet.trat.all.lm.rm.cbo <- lm(ttlpltrmcbo$Freq~ttlpltrmcbo$Treatment) 
testet.trat.all.lm.rm.cbo
summary(testet.trat.all.lm.rm.cbo)





### grafico de recrutamento por tratamento, Itamambuca

ttlpltrmita <- ttlpltrm %>% 
  filter(`Site` == "ITA")
ttlpltrmita


ggplot(ttlpltrmita, aes(x=Period, y=Freq, fill = Treatment)) + 
  geom_bar(position = position_dodge(), stat = "identity", alpha = 0.8) + #geom_hline(yintercept = median(ttlplt$Freq, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  theme_bw() + scale_fill_manual(name = "Treatment", labels = c("Closed", "Open"), values=c("slateblue1","tomato")) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        #panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        #panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.88, .98),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) + 
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100)) +
  labs(x = "Time (month)", y = "Recruitment before mortality (Itamambuca)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                            "54","60","66","72","78","84"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_year_ita_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado ITA

ggplot(ttlpltrmita, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment before mortality (Itamambuca)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = 0.01, P-value = 0.2566", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento_mortalidade/recrut_antes_mort_all_treatm_ita_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrmita$Freq)
testet.trat.all.rm.ita <- t.test(ttlpltrmita$Freq~ttlpltrmita$Treatment)
testet.trat.all.rm.ita
testet.trat.all.lm.rm.ita <- lm(ttlpltrmita$Freq~ttlpltrmita$Treatment) 
testet.trat.all.lm.rm.ita
summary(testet.trat.all.lm.rm.ita)


