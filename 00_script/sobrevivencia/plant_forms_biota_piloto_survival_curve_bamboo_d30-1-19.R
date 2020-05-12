library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")
library("tidyquant")  # Loads tidyverse, tidquant, financial pkgs, xts/zoo
library("cranlogs")   # For inspecting package downloads over year
library("survival")


### Cardoso

biota_e_edulis<- as.data.frame(read.csv("Life_Form_valesca_30-1-19_surv.csv"))
biota_e_edulis
biota_e_edulis$surv <- biota_e_edulis$Tdeath - biota_e_edulis$Trecruit
biota_e_edulis

biota_e_edulis <- biota_e_edulis %>% 
  filter(`Site` == "CAR" & `Life.Form` == "bamboo") 
biota_e_edulis


attach(biota_e_edulis)
names(biota_e_edulis)
#Gr?fico curva de sobreviv?ncia
plot(survfit(Surv(surv,alive)~Treatment), col = c(1,2), axes= FALSE, ylab="Survival probability of seedlings Bamboos", xlab="Time (months)", 
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



#### Itamambuca


biota_e_edulis<- as.data.frame(read.csv("Life_Form_valesca_30-1-19_surv.csv"))
biota_e_edulis
biota_e_edulis$surv <- biota_e_edulis$Tdeath - biota_e_edulis$Trecruit
biota_e_edulis

biota_e_edulis <- biota_e_edulis %>% 
  filter(`Site` == "ITA" & `Life.Form` == "bamboo") 
biota_e_edulis


attach(biota_e_edulis)
names(biota_e_edulis)
#Gr?fico curva de sobreviv?ncia
plot(survfit(Surv(surv,alive)~Treatment), col = c(1,2), axes= FALSE, ylab="Survival probability of seedlings Bamboos", xlab="Time (months)", 
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





#### CBO

biota_e_edulis<- as.data.frame(read.csv("Life_Form_valesca_30-1-19_surv.csv"))
biota_e_edulis
biota_e_edulis$surv <- biota_e_edulis$Tdeath - biota_e_edulis$Trecruit
biota_e_edulis

biota_e_edulis <- biota_e_edulis %>% 
  filter(`Site` == "CBO" & `Life.Form` == "bamboo") 
biota_e_edulis


attach(biota_e_edulis)
names(biota_e_edulis)
#Gr?fico curva de sobreviv?ncia
plot(survfit(Surv(surv,alive)~Treatment), col = c(1,2), axes= FALSE, ylab="Survival probability of seedlings Bamboos", xlab="Time (months)", 
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





#### VGM

biota_e_edulis<- as.data.frame(read.csv("Life_Form_valesca_30-1-19_surv.csv"))
biota_e_edulis
biota_e_edulis$surv <- biota_e_edulis$Tdeath - biota_e_edulis$Trecruit
biota_e_edulis

biota_e_edulis <- biota_e_edulis %>% 
  filter(`Site` == "VGM" & `Life.Form` == "bamboo") 
biota_e_edulis


attach(biota_e_edulis)
names(biota_e_edulis)
#Gr?fico curva de sobreviv?ncia
plot(survfit(Surv(surv,alive)~Treatment), col = c(1,2), axes= FALSE, ylab="Survival probability of seedlings Bamboos", xlab="Time (months)", 
     lty=c(2,1,1,2))
axis(side=2)
axis(side=1, seq(0,84,by=6))
#Adicionar legenda
legend(x=20,y=1, title= "Treatment", col = c(2,1), legend = c("Open","Close"),cex=0.8,lty=c(1,2,1,2))           
box()

#Tabela com o c?lculo da probabilidade de sobreviv?ncia ao longo do tempo para cada tratamento
model1<-survfit(Surv(surv,alive)~Treatment)
summary(model1) 

#An?lise com modelo Cox para verificar efeito da intera??o entre localidade de semeadura e origem da semente sobre a sobreviv?ncia de pl?ntulas
model2<-coxph(Surv(surv,alive)~Treatment)
summary(model2) 



### All areas
biota_e_edulis<- as.data.frame(read.csv("Life_Form_valesca_30-1-19_surv.csv"))
biota_e_edulis
biota_e_edulis$surv <- biota_e_edulis$Tdeath - biota_e_edulis$Trecruit
biota_e_edulis

biota_e_edulis <- biota_e_edulis %>% 
  filter(`Life.Form` == "bamboo") 
biota_e_edulis


attach(biota_e_edulis)
names(biota_e_edulis)
#Gr?fico curva de sobreviv?ncia
plot(survfit(Surv(surv,alive)~Treatment), col = c(1,2), axes= FALSE, ylab="Survival probability of seedlings Bamboos", xlab="Time (months)", 
     lty=c(2,1,1,2))
axis(side=2)
axis(side=1, seq(0,84,by=6))
#Adicionar legenda
legend(x=20,y=1, title= "Treatment", col = c(2,1), legend = c("Open","Close"),cex=0.8,lty=c(1,2,1,2))           
box()

#Tabela com o c?lculo da probabilidade de sobreviv?ncia ao longo do tempo para cada tratamento
model1<-survfit(Surv(surv,alive)~Treatment)
summary(model1) 

#An?lise com modelo Cox para verificar efeito da intera??o entre localidade de semeadura e origem da semente sobre a sobreviv?ncia de pl?ntulas
model2<-coxph(Surv(surv,alive)~Treatment)
summary(model2)
