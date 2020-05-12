### YURI - 08-08-19
### Script para grafico de recrutamento de individuos que continuam vivos após a primeira amostragem

library(tidyverse)
library(beepr)

setwd("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/recrutamento/")


ttlpltr <- read_csv("recrutamento_por_amostragem_08-08-19.csv")
ttlpltr


### grafico de recrutamento por tratamento, todas as áreas

ggplot(ttlpltr, aes(x=Period, y=Freq, fill = Treatment)) + 
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
  labs(x = "Time (month)", y = "Recruitment (All Areas)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                          "54","60","66","72","78","84","90","96"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_year_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado

ggplot(ttlpltr, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment (All Areas)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = 0.01, P-value = 0.1085", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_treatm_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltr$Freq)
testet.trat.all.rm <- t.test(ttlpltr$Freq~ttlpltr$Treatment)
testet.trat.all.rm
testet.trat.all.lm.r <- lm(ttlpltr$Freq~ttlpltr$Treatment) 
testet.trat.all.lm.r
summary(testet.trat.all.lm.r)



### grafico de recrutamento por tratamento, CARDOSO

ttlpltrcar <- ttlpltr %>% 
  filter(`Site` == "CAR")
ttlpltrcar


ggplot(ttlpltrcar, aes(x=Period, y=Freq, fill = Treatment)) + 
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
  labs(x = "Time (month)", y = "Recruitment (Cardoso Island)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                       "54","60","66","72","78","84"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_year_car_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado CAR

ggplot(ttlpltrcar, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment (Cardoso Island)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = 0.02, P-value = 0.1844", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_treatm_car_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrcar$Freq)
testet.trat.all.r.car <- t.test(ttlpltrcar$Freq~ttlpltrcar$Treatment)
testet.trat.all.r.car
testet.trat.all.lm.r.car <- lm(ttlpltrcar$Freq~ttlpltrcar$Treatment) 
testet.trat.all.lm.r.car
summary(testet.trat.all.lm.r.car)





### grafico de recrutamento por tratamento, Vargem Grande

ttlpltrvgm <- ttlpltr %>% 
  filter(`Site` == "VGM")
ttlpltrvgm


ggplot(ttlpltrvgm, aes(x=Period, y=Freq, fill = Treatment)) + 
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
  labs(x = "Time (month)", y = "Recruitment (Vargem Grande)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                            "54","60","66","72","78","84"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_year_vgm_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado VGM

ggplot(ttlpltrvgm, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment (Vargem Grande)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = -0.01, P-value = 0.4507", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_treatm_vgm_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrvgm$Freq)
testet.trat.all.r.vgm <- t.test(ttlpltrvgm$Freq~ttlpltrvgm$Treatment)
testet.trat.all.r.vgm
testet.trat.all.lm.r.vgm <- lm(ttlpltrvgm$Freq~ttlpltrvgm$Treatment) 
testet.trat.all.lm.r.vgm
summary(testet.trat.all.lm.r.vgm)





### grafico de recrutamento por tratamento, Carlos Botelho

ttlpltrcbo <- ttlpltr %>% 
  filter(`Site` == "CBO")
ttlpltrcbo


ggplot(ttlpltrcbo, aes(x=Period, y=Freq, fill = Treatment)) + 
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
        legend.position = c(0.80, .98),
        legend.justification = c("left", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)) + 
  expand_limits(y=100) + scale_y_continuous(breaks=c(0,50,100)) +
  labs(x = "Time (month)", y = "Recruitment (Carlos Botelho)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                           "54","60","66","72","78","84","90","96","102","108"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_year_cbo_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado CBO

ggplot(ttlpltrcbo, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment (Carlos Botelho)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = -0.03, P-value = 0.8242", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_treatm_cbo_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrcbo$Freq)
testet.trat.all.r.cbo <- t.test(ttlpltrcbo$Freq~ttlpltrcbo$Treatment)
testet.trat.all.r.cbo
testet.trat.all.lm.r.cbo <- lm(ttlpltrcbo$Freq~ttlpltrcbo$Treatment) 
testet.trat.all.lm.r.cbo
summary(testet.trat.all.lm.r.cbo)





### grafico de recrutamento por tratamento, Itamambuca

ttlpltrita <- ttlpltr %>% 
  filter(`Site` == "ITA")
ttlpltrita


ggplot(ttlpltrita, aes(x=Period, y=Freq, fill = Treatment)) + 
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
  labs(x = "Time (month)", y = "Recruitment (Itamambuca)") + scale_x_discrete(limits=c("0","06","12","18","24","30","36","42","48",
                                                                                                            "54","60","66","72","78","84"))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_year_ita_07-08-19.JPEG", he = 15, wi = 30, un = "cm", dpi = 300)



### Grafico de comparacao total entre aberto fechado ITA

ggplot(ttlpltrita, aes(x=Treatment, y=Freq, fill = Treatment)) +
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
  labs(x = "Treatment", y = "Recruitment (Itamambuca)") +
  expand_limits(y=150) + scale_y_continuous(breaks=c(0,50,100,150)) +
  annotate("text", label = "R² = 0.15, P-value = 0.01", size = 5, x = 1.0, y = 150)

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/padrao/recrutamento/recrut_all_treatm_ita_07-08-19.JPEG", he = 15, wi = 15, un = "cm", dpi = 300)

### Anova e test-t para todas as áreas
shapiro.test(ttlpltrita$Freq)
testet.trat.all.r.ita <- t.test(ttlpltrita$Freq~ttlpltrita$Treatment)
testet.trat.all.r.ita
testet.trat.all.lm.r.ita <- lm(ttlpltrita$Freq~ttlpltrita$Treatment) 
testet.trat.all.lm.r.ita
summary(testet.trat.all.lm.r.ita)


