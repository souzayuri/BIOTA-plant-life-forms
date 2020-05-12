library(RColorBrewer)
library(tidyverse)
install.packages("gplots")
library(gplots)
library(RColorBrewer)


setwd("~/BIOTA-PARCELAS")

da <- read.table("data_all_areas_mammals_biota_heatmap.txt", header = TRUE, sep = "\t", row.names = 1)
data=as.matrix(da)
head(data)
data <- sqrt(data)

heatmap(data)

heatmap(data)
heatmap.2(data, density.info="none", trace="none", 
          col=my_palette, breaks=col_breaks,  
          dendrogram="both")   
          
?heatmap.2          
col_breaks = c(seq(0,0.01,length=100), # for red
               seq(0.02,30,length=100),  # for yellow
               seq(31,150,length=100), #for green
               seq(151,700, length =100)) # for blue


my_palette <- colorRampPalette(c("red", "yellow", "green", "blue"))(n = 399)



