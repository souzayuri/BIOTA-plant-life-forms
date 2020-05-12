### Contact: Yuri (yuri.eco2013@gmail.com)
### Data: 08-08-2019
### Description: Regressão linear do dossel com numero total de plantulas total e por áreas
### Content: regressão linear e geraçào de gráfico 

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("car")) install.packages("car", dependencies = TRUE)
if(!require("broom")) install.packages("broom", dependencies = TRUE)


rm(list = ls()) 


dossel <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/dossel/dossel_plantulas_08-08-19.csv")
dossel


### Regressao Linear - total

# normalidade da variavel resposta
shapiro.test(dossel$total_plantulas)

plot(density(dossel$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm <- lm(total_plantulas ~ dossel, data = dossel)
ttl.dos.plt.lm
summary(ttl.dos.plt.lm)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm$residuals)

# gráfico total áreas dossel
ggplot(data = dossel) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover (%)",
       y = "Seedlings") +
  annotate("text", label = "R² = -0.01, P-value = 0.85", size = 5, x = 65, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/ttl_dossel_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)



### Regressao Linear - total - aberto

dossel.ttl.o <- dossel %>% 
  filter(`tratamento` == "open")
dossel.ttl.o 

# normalidade da variavel resposta
shapiro.test(dossel.ttl.o$total_plantulas)

plot(density(dossel.ttl.o$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.o$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.o <- lm(total_plantulas ~ dossel, data = dossel.ttl.o)
ttl.dos.plt.lm.o
summary(ttl.dos.plt.lm.o)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.o$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.o) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover (%) (Open)",
       y = "Seedlings (Open)") +
  annotate("text", label = "R² = -0.02, P-value = 0.86", size = 5, x = 65, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/ttl_dossel_open_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)




### Regressao Linear - total - fechado

dossel.ttl.c <- dossel %>% 
  filter(`tratamento` == "closed")
dossel.ttl.c 

# normalidade da variavel resposta
shapiro.test(dossel.ttl.c$total_plantulas)

plot(density(dossel.ttl.c$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.c$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.c <- lm(total_plantulas ~ dossel, data = dossel.ttl.c)
ttl.dos.plt.lm.c
summary(ttl.dos.plt.lm.c)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.c$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.c) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover (%) (Closed)",
       y = "Seedlings (Closed)") +
  annotate("text", label = "R² = -0.02, P-value = 0.77", size = 5, x = 65, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/ttl_dossel_closed_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)




### Regressao Linear - CAR - aberto

dossel.ttl.o.car <- dossel %>% 
  filter(`Parcelas` == "CAR", `tratamento` == "open")
dossel.ttl.o.car

# normalidade da variavel resposta
shapiro.test(dossel.ttl.o.car$total_plantulas)

plot(density(dossel.ttl.o.car$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.o.car$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.o.car <- lm(total_plantulas ~ dossel, data = dossel.ttl.o.car)
ttl.dos.plt.lm.o.car
summary(ttl.dos.plt.lm.o.car)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.o.car$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.o.car) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - CAR (%) (Open)",
       y = "Seedlings - CAR (Open)") +
  annotate("text", label = "R² = 0.16, P-value = 0.10", size = 5, x = 65, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/car_dossel_open_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)




### Regressao Linear - CAR - fechado

dossel.ttl.c.car <- dossel %>% 
  filter(`Parcelas` == "CAR", `tratamento` == "closed")
dossel.ttl.c.car

# normalidade da variavel resposta
shapiro.test(dossel.ttl.c.car$total_plantulas)

plot(density(dossel.ttl.c.car$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.c.car$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.c.car <- lm(total_plantulas ~ dossel, data = dossel.ttl.c.car)
ttl.dos.plt.lm.c.car
summary(ttl.dos.plt.lm.c.car)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.c.car$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.c.car) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - CAR (%) (Closed)",
       y = "Seedlings - CAR (Closed)") +
  annotate("text", label = "R² = 0.10, P-value = 0.15", size = 5, x = 65, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/car_dossel_closed_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)





### Regressao Linear - CBO - aberto

dossel.ttl.o.cbo <- dossel %>% 
  filter(`Parcelas` == "CBO", `tratamento` == "open")
dossel.ttl.o.cbo

# normalidade da variavel resposta
shapiro.test(dossel.ttl.o.cbo$total_plantulas)

plot(density(dossel.ttl.o.cbo$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.o.cbo$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.o.cbo <- lm(total_plantulas ~ dossel, data = dossel.ttl.o.cbo)
ttl.dos.plt.lm.o.cbo
summary(ttl.dos.plt.lm.o.cbo)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.o.cbo$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.o.cbo) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - CBO (%) (Open)",
       y = "Seedlings - CBO (Open)") +
  annotate("text", label = "R² = -0.11, P-value = 0.84", size = 5, x = 65.6, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/cbo_dossel_open_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)




### Regressao Linear - CBO - fechado

dossel.ttl.c.cbo <- dossel %>% 
  filter(`Parcelas` == "CBO", `tratamento` == "closed")
dossel.ttl.c.cbo

# normalidade da variavel resposta
shapiro.test(dossel.ttl.c.cbo$total_plantulas)

plot(density(dossel.ttl.c.cbo$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.c.cbo$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.c.cbo <- lm(total_plantulas ~ dossel, data = dossel.ttl.c.cbo)
ttl.dos.plt.lm.c.cbo
summary(ttl.dos.plt.lm.c.cbo)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.c.cbo$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.c.cbo) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - CBO (%) (Closed)",
       y = "Seedlings - CBO (Closed)") +
  annotate("text", label = "R² = -0.11, P-value = 0.76", size = 5, x = 63, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/cbo_dossel_closed_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)





### Regressao Linear - VGM - aberto

dossel.ttl.o.vgm <- dossel %>% 
  filter(`Parcelas` == "VGM", `tratamento` == "open")
dossel.ttl.o.vgm

# normalidade da variavel resposta
shapiro.test(dossel.ttl.o.vgm$total_plantulas)

plot(density(dossel.ttl.o.vgm$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.o.vgm$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.o.vgm <- lm(total_plantulas ~ dossel, data = dossel.ttl.o.vgm)
ttl.dos.plt.lm.o.vgm
summary(ttl.dos.plt.lm.o.vgm)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.o.vgm$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.o.vgm) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - VGM (%) (Open)",
       y = "Seedlings - VGM (Open)") +
  annotate("text", label = "R² = 0.16, P-value = 0.10", size = 5, x = 65.9, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/vgm_dossel_open_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)




### Regressao Linear - VGM - fechado

dossel.ttl.c.vgm <- dossel %>% 
  filter(`Parcelas` == "VGM", `tratamento` == "closed")
dossel.ttl.c.vgm

# normalidade da variavel resposta
shapiro.test(dossel.ttl.c.vgm$total_plantulas)

plot(density(dossel.ttl.c.vgm$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.c.vgm$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.c.vgm <- lm(total_plantulas ~ dossel, data = dossel.ttl.c.vgm)
ttl.dos.plt.lm.c.vgm
summary(ttl.dos.plt.lm.c.vgm)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.c.vgm$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.c.vgm) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - VGM (%) (Closed)",
       y = "Seedlings - VGM (Closed)") +
  annotate("text", label = "R² = -0.09, P-value = 0.90", size = 5, x = 66.5, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/vgm_dossel_closed_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)





### Regressao Linear - ITA - aberto

dossel.ttl.o.ita <- dossel %>% 
  filter(`Parcelas` == "ITA", `tratamento` == "open")
dossel.ttl.o.ita

# normalidade da variavel resposta
shapiro.test(dossel.ttl.o.ita$total_plantulas)

plot(density(dossel.ttl.o.ita$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.o.ita$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.o.ita <- lm(total_plantulas ~ dossel, data = dossel.ttl.o.ita)
ttl.dos.plt.lm.o.ita
summary(ttl.dos.plt.lm.o.ita)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.o.ita$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.o.ita) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - ITA (%) (Open)",
       y = "Seedlings - ITA (Open)") +
  annotate("text", label = "R² = -0.13, P-value = 0.83", size = 5, x = 65, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/ita_dossel_open_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)




### Regressao Linear - ITA - fechado

dossel.ttl.c.ita <- dossel %>% 
  filter(`Parcelas` == "ITA", `tratamento` == "closed")
dossel.ttl.c.ita

# normalidade da variavel resposta
shapiro.test(dossel.ttl.c.ita$total_plantulas)

plot(density(dossel.ttl.c.ita$total_plantulas), ylab = "Densidade", # Vendo a distribuição da curva de n_sp
     xlab = "Número de plantulas", main = "")

# O gráfico quantil-quantil (q-q) para checar adequação de distribuição de frequência dos dados à uma distribuição de probabilidades
qqPlot(dossel.ttl.c.ita$total_plantulas, pch = 20, dist = "norm", envelope = .95)

# teste de regressão linear simples
ttl.dos.plt.lm.c.ita <- lm(total_plantulas ~ dossel, data = dossel.ttl.c.ita)
ttl.dos.plt.lm.c.ita
summary(ttl.dos.plt.lm.c.ita)

# normalidade dos residuos
shapiro.test(ttl.dos.plt.lm.c.ita$residuals)

# gráfico total áreas dossel
ggplot(data = dossel.ttl.c.ita) +
  aes(x = dossel, y = total_plantulas, color = tratamento) +
  geom_point(shape = 21, size = 5, col = "black", fill = "forestgreen") +
  stat_smooth(method = "lm", col = "black", level = .95) +
  theme_bw() +
  labs(x = "Canopy cover - ITA (%) (Closed)",
       y = "Seedlings - ITA (Closed)") +
  annotate("text", label = "R² = 0.48, P-value = 0.02", size = 5, x = 74.3, y = 120) +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  expand_limits(y=120) + scale_y_continuous(breaks=c(10,20,30,40,50,60,70,80,90,100,110,120))

ggsave("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/dossel/ita_dossel_closed_08-08-19.JPEG", he = 15, wi = 20, un = "cm", dpi = 300)
