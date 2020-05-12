
rm(list=ls())
library(vegan)

dataW<-read.table("Matrix_functional.txt",header = TRUE, dec=",", na.strings = "na")
summary(dataW)
dataW.matrix<-as.matrix(dataW[,3:13])##response variables in a sample x species matrix
dataW.matrix<-sqrt(dataW.matrix)#square root transform
summary(dataW.matrix)

#COMPOSITIONAL DIFFERENCES
dataW.dist<-vegdist(dataW.matrix, method='bray')#create dissimilarity matrix (Bray-Curtis distance to centroid)

#Significant differences between communities between different dates
dataW.div.date<-adonis2(dataW.dist~Date, data=dataW, permutations = 999, method="bray")
dataW.div.date

#Significant detween communities between different sites
dataW.div.site<-adonis2(dataW.dist~site, data=dataW, permutations = 999, method="bray")
dataW.div.site

#Significant detween communities between different sites and dates
dataW.div.int<-adonis2(dataW.dist~site*Date, data=dataW, permutations = 999, method="bray")
dataW.div.int

#extract the PCA axes and variance explained
dataW.pca<-prcomp(dataW.matrix, scale = FALSE)
dataW.pca #loadings
summary(dataW.pca) #variance explained



#BETA DIVERSITY
dataW.dist<-vegdist(dataW.matrix, method='bray')#create dissimilarity matrix (Bray-Curtis distance to centroid)

#Non-significant differences between variances of communities grouped by dates (beta diversity constant across time)
dispersion.Date<-betadisper(dataW.dist, group=dataW$Date)
permutest(dispersion.Date)
plot(dispersion.Date, hull=FALSE, ellipse=TRUE) ##sd ellipse

#Significant differences between variances of communities grouped by sites (some sites have higher beta diversity than others)
dispersion.site<-betadisper(dataW.dist, group=dataW$site)
permutest(dispersion.site)
plot(dispersion.site, hull=FALSE, ellipse=TRUE) ##sd ellipse


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
