### Script para os graficos de piramide etária
## Yuri 6-5-19

library(plotrix)
library(tidyverse)


gf <- read.csv("life_form_piramide_etaria_06-06-19.csv")
gf <- gf[-c(2,7:60),]
gf <- gf[,-c(1:35)]


open <- gf$frequencepct_total_open
close <- gf$frequencepct_total_close
time <- gf$Period
open
close
time

mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
par(mar=pyramid.plot(open,close,labels=time,top.labels=c("Open","Period (months)","Close"),
                     main="Total number of seedlings in each treatment",
                     lxcol="tan4",rxcol="darkgreen",unit="Number of Individuals (%)",
                     gap=2, show.values=FALSE))

