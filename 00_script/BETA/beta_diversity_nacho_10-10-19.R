
rm(list=ls())
library(vegan)
library(tidyverse)
library(stringr)
library(beepr)


### Fitting the data

bt.dt <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/life_form_yuri_2019v2.csv")
bt.dt 

bt.dt.slc <- bt.dt [,-c(1,5,23:28)]
bt.dt.slc

bt.dt.slc.gtr <- bt.dt.slc %>% 
  gather(key = "time", value = "value", 4:20)
bt.dt.slc.gtr

total.lf <- bt.dt.slc.gtr %>% 
  group_by(Site, Plot, Treatment, time, `Life Form`) %>% 
  summarise(abundancia = sum(value)) %>% 
  rename(life_form = `Life Form`, month = time) %>% # renomeia a coluna `Life Form`
  mutate(time = as.numeric(month %>% stringr::str_replace("p", ""))) %>% # copia a coluna de tempo e transforma em numerico
  spread(life_form, abundancia) #converte para widescreen
total.lf
total.lf[is.na(total.lf)] <- 0 # substitui onde e NA por 0
total.lf



#dataW<-read.table("Matrix_functional.txt",header = TRUE, dec=",", na.strings = "na")
#summary(total.lf)
dataW.matrix<-as.matrix(total.lf[,c(6,7,9:12)])##response variables in a sample x species matrix
dataW.matrix<-sqrt(dataW.matrix)#square root transform
summary(dataW.matrix)

#COMPOSITIONAL DIFFERENCES
dataW.dist<-vegdist(dataW.matrix, method='bray')#create dissimilarity matrix (Bray-Curtis distance to centroid)
summary(dataW.dist)
names(dataW.dist)
glimpse(dataW.dist)

#Significant differences between communities between different dates
dataW.div.date<-adonis2(dataW.dist~month, data=total.lf, permutations = 999, method="bray")
dataW.div.date
beep(1)


#Significant detween communities between different sites
dataW.div.site<-adonis2(dataW.dist~Treatment, data=total.lf, permutations = 999, method="bray")
dataW.div.site
beep(4)


#Significant detween communities between different sites and dates
dataW.div.int<-adonis2(dataW.dist~month*Treatment, data=total.lf, permutations = 999, method="bray", strata = "Site")
dataW.div.int
beep(2)



#extract the PCA axes and variance explained
dataW.pca<-prcomp(dataW.matrix, scale = FALSE)
dataW.pca #loadings
summary(dataW.pca) #variance explained

fviz_pca_biplot(dataW.pca,
                # Individuals
                geom.ind = "point",
                fill.ind = total.lf$month, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                title = "Contributions of all variables by treatment (PCA - Biplot)",
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                legend.title = list(fill = "Treatment", color = "Contribution",
                                    alpha = "Contribution"))

#BETA DIVERSITY
dataW.dist<-vegdist(dataW.matrix, method='bray')#create dissimilarity matrix (Bray-Curtis distance to centroid)

#Non-significant differences between variances of communities grouped by dates (beta diversity constant across time)
dispersion.Date<-betadisper(dataW.dist, group=total.lf$month)
permutest(dispersion.Date)
plot(dispersion.Date, hull=FALSE, ellipse=TRUE) ##sd ellipse

#Significant differences between variances of communities grouped by sites (some sites have higher beta diversity than others)
dispersion.treatment<-betadisper(dataW.dist, group=total.lf$Treatment*total.lf$month)
permutest(dispersion.treatment)
plot(dispersion.treatment, hull=FALSE, ellipse=TRUE) ##sd ellipse


#ALPHA DIVERSITY
H <- diversity(dataW.matrix) #Shanon
simp <- diversity(dataW.matrix, "simpson") #Simpson
S <- specnumber(dataW.matrix)
J <- H/log(dataW.matrix)
dataW$H<-H #add results as a new column
dataW$simp<-simp #add results as a new column
dataW$S<-S #add results as a new column
dataW$J<-J #add results as a new column

#significant differnces in alpha diversity between dates
res.aov.date<-aov(H~Date,data=dataW)
summary(res.aov.date)
res.aov.date<-aov(S~Date,data=dataW)
summary(res.aov.date)
boxplot(dataW$S~dataW$Date)

#Non-significant differnces in alpha diversity between sites
res.aov.site<-aov(H~site,data=dataW)
summary(res.aov.site)
res.aov.site<-aov(S~site,data=dataW)
summary(res.aov.site)
boxplot(dataW$S~dataW$site)

#Non-significant interaction
res.aov.site<-aov(H~site*Date,data=dataW)
summary(res.aov.site)
res.aov.site<-aov(S~site*Date,data=dataW)
summary(res.aov.site)
