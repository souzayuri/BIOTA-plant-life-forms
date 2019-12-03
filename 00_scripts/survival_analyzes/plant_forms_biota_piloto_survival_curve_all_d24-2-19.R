library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")
library("tidyquant")  # Loads tidyverse, tidquant, financial pkgs, xts/zoo
library("cranlogs")   # For inspecting package downloads over year
library("survival")
library("survminer")


biota_e_edulis<- as.data.frame(read.csv("Life_Form_valesca_30-1-19_surv.csv"))
biota_e_edulis
biota_e_edulis$surv <- biota_e_edulis$Tdeath - biota_e_edulis$Trecruit
biota_e_edulis

biota_e_edulis <- subset(biota_e_edulis, surv >= 0) # tirar uma parte dos dados com alguma restricao
biota_e_edulis

biota_e_edulis <- biota_e_edulis %>% 
  filter(`Site` == "CAR") 
biota_e_edulis


attach(biota_e_edulis)
names(biota_e_edulis)
#Gr?fico curva de sobreviv?ncia
plot(survfit(Surv(surv,alive)~Treatment), col = c(1,2), axes= FALSE, ylab="Probabilidade de sobrevivência - CAR", xlab="Tempo (meses)", 
     lty=c(2,1,1,2))
axis(side=2)
axis(side=1, seq(0,84,by=6))
#Adicionar legenda
legend(x=20,y=1, title= "Source-Transplant site", col = c(2,1), legend = c("Open","Close"),cex=0.8,lty=c(1,2,1,2))           
box()


#Tabela com o c?lculo da probabilidade de sobreviv?ncia ao longo do tempo para cada tratamento
model1<-survfit(Surv(surv,alive)~Treatment)
summary(model1) 

#An?lise com modelo Cox para verificar efeito da intera??o entre localidade de semeadura e origem da semente sobre a sobreviv?ncia de pl?ntulas
model2<-coxph(Surv(surv,alive)~Treatment)
summary(model2)    

#Como n?o deu intera??o, usei o modelo sem intera??o
#An?lise do modelo Cox sem intera??o
model3<-coxph(Surv(Surv(surv,alive)~Treatment+Site))
summary(model3)



#GGPLOT2

fit <- survfit(Surv(surv,alive)~Treatment, data = biota_e_edulis)
ggsurvplot(fit, data = biota_e_edulis, censor.shape="|", censor.size = 2)


ggsurvplot(
  fit,
  data = biota_e_edulis,
  size = 1,                 # change line size
  palette = 
    c("#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = 
    c("Closed","Open"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
)



table.

ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = biota_e_edulis,             # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimates of survival curves.
  xlim = c(0,84),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Tempo (meses)",   # customize X axis label.
  ylab = "Probabilidade de sobrevivência",
  break.time.by = 6,     # break X axis in time intervals by 500.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  
  
  
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
  # in legend of risk table
)



biota_e_edulis<- as.data.frame(read.csv("Life_Form_valesca_30-1-19_surv.csv"))
biota_e_edulis
biota_e_edulis$surv <- biota_e_edulis$Tdeath - biota_e_edulis$Trecruit
biota_e_edulis

biota_e_edulis <- subset(biota_e_edulis, surv >= 0) # tirar uma parte dos dados com alguma restricao
biota_e_edulis

#biota_e_edulis <- biota_e_edulis %>% 
#  filter(`Site` == "ITA") 
#biota_e_edulis

fit <- survfit(Surv(surv,alive)~Treatment, data = biota_e_edulis)
ggsurvplot(fit, data = biota_e_edulis, censor.shape="|", censor.size = 2)

ggsurv <- ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = biota_e_edulis,   # data used to fit survival curves.
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimates of survival curves.
  palette = c("#E7B800", "#2E9FDF"),
  xlim = c(0,84),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time (Month)",   # customize X axis label.
  ylab = "Probability of survival",
  break.time.by = 6,     # break X axis in time intervals by 6.
  ggtheme = theme_bw(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.height = 0.25, # the height of the risk table
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
    # in legend of risk table.
    ncensor.plot = F,      # plot the number of censored subjects at time t
  ncensor.plot.height = 0.25,
  conf.int.style = "step",  # customize style of confidence intervals
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs =
    c("Exclusion","Opened"))    # change legend labels.

ggsurv$plot <- ggsurv$plot + labs(
  subtitle = "Survival curves")
ggsurv

ggsurv$table <- ggsurv$table + labs(
  title = "Number of survivors")
ggsurv


# Changing the font size, style and color
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Applying the same font style to all the components of ggsurv:
# survival curves, risk table and censor part

ggsurv <- ggpar(
  ggsurv,
  font.title = c(12, "black"),
  font.x = c(12, "black"),          
  font.y = c(12, "black"))

ggsurv$table <- ggpar(
  ggsurv$table,
  font.y = c(0, "white"))
ggsurv




