
library(gplots)
library(vegan)
#Definindo diretório


#Carregando os dados
heatplant <- read.table("data_plant_life_heatmap_ordered_d17_04_2019.txt", header = TRUE, sep = "\t", row.names = 1)
str(heatplant)

heatplant  <- log10(heatplant +1)

#Transformando os dados em matriz para a função heatmap.2
heatplant <- as.matrix(heatplant)
head(heatplant )

heatmap(heatplant)
heatmap(heatplant, scale="column")


my_palette <- colorRampPalette(c("darkred", "yellow", "green", "blue"))(n =399)


#png(filename = "heatmap_biomass_mammals_biota2.png", width = 5*300, height = 5*300, res = 300, pointsize = 8)

heatmap.2(heatplant, trace= "none", density.info = "none", col=my_palette,
          RowSideColors = c(rep("gray", 43), rep("black", 43)), cexRow = .5)

par(lend = 1)           # square line ends for the color legend
legend("bottom",      # location of the legend on the heatmap plot
       legend = c("Open", "Closed"), # category labels
       col = c("gray", "black"),  # color key
       lty= 1,             # line style
       lwd = 10            # line width
)
#dev.off()


pushViewport(viewport(width = 0.9, height = 0.9))
grid.rect()  # border
