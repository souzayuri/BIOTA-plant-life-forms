#Script heatmap, Biomassa/dia mamíferos parcelas biota dados de 2017
#21.01.2019
#Paula Akkawi - paulakkawi@gmail.com

#Instalando pacote
install.packages("gplots")
library(gplots)

#Definindo diretório
setwd("~/BIOTA-PARCELAS")

#Carregando os dados
da <- read.table("data_all_areas_mammals_biota_heatmap.txt", header = TRUE, sep = "\t", row.names = 1)
str(da)

#Tirando o logaritimo. Somei um 1 para evitar o infinito do log de zero
data_log <- log10(da+1)

#Transformando os dados em matriz para a função heatmap.2
data <- as.matrix(data_log)


#Carregando as cores

my_palette <- colorRampPalette(c("darkred", "yellow", "green", "blue"))(n =399 )


#Gerando o arquivo da imagem
png(filename = "heatmap_biomass_mammals_biota3.png", width = 5*300, height = 5*300, res = 300, pointsize = 8)

#Gerando o heatmap
heatmap.2(data, trace= "none", density.info = "none", col=my_palette,
          RowSideColors = c(rep("gray", 43), 
                              rep("black", 43)), cexRow = .5)


#Salvando a imagem na pasta do diretório
dev.off()
