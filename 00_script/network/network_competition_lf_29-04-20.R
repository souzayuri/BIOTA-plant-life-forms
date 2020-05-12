# Informations ------------------------------------------------------------

### title: Network competition ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 29/04/2020
### Description: this script prepare the date to plot a network competition life form interaction

rm(list = ls())



# load packages ------------------------------------------------------------



library(tidyverse)
library(scales)
library(GGally)
library(network)
library(geomnet)
library(ggnetwork)
library(sna)
library(igraph)



# examples ----------------------------------------------------------------

nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
nodes

links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
links


# open plots --------------------------------------------------------------



# links
biota.link <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/network/life_forms_competition_estimates_28-04-20_open_sig.csv")
biota.link$effect <- as.factor(biota.link$effect)


#nodes
biota.node <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/network/abundance_total_lf_network_28-04-20_open.csv")
biota.node$Abundances_log <- log(biota.node$Abundances)


net.biota <- graph_from_data_frame(d=biota.link, vertices=biota.node, directed=T)
net.biota

plot(net.biota, vertex.label=NA)

net.biota <- simplify(net.biota, remove.multiple = F, remove.loops = T) 

plot(net.biota, edge.arrow.size=.4,vertex.label=NA, edge.curved=.1)

corls.biota <- c("springgreen4","khaki4",
                 "yellowgreen","tan1","sienna3","brown")
V(net.biota)$color <- corls.biota[V(net.biota)$type]


deg.biota <- degree(net.biota, mode="all")
V(net.biota)$size <- deg.biota*3
plot(net.biota) 


V(net.biota)$size <- V(net.biota)$Abundances_log*8
plot(net.biota) 


V(net.biota)$label <- NA

E(net.biota)$width <- E(net.biota)$estimate*10
plot(net.biota) 

E(net.biota)$arrow.size <- 2
E(net.biota)$edge.color <- "gray80"

graph_attr(net.biota, "layout") <- layout_in_circle
plot(net.biota, edge.curved=.1) 


tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/network/log_network_sig_lf_open_29-04-20.tif", width = 15, height = 15, units = 'in', res = 500)

plot(net.biota, edge.curved=.1, vertex.color="gray90", vertex.frame.color	= c("brown","sienna3",
     "khaki4","yellowgreen","tan1","springgreen4"))  

                               
dev.off()






ggplot(net.biota, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(size = estimate, color = effect),
    curvature = 0.1) + 
  geom_nodes(aes(color = life_form)) +
  geom_nodelabel(aes(label = abundance, color = life_form)) +
  labs(title="Rice trade", subtitle="Arrows in direction of goods") +
  theme_void()




# closed plots ------------------------------------------------------------




# links
biota.link <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/network/life_forms_competition_estimates_28-04-20_closed_sig.csv")
biota.link$effect <- as.factor(biota.link$effect)


#nodes
biota.node <- read_csv("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/network/abundance_total_lf_network_28-04-20_closed.csv")
biota.node$Abundances_log <- log(biota.node$Abundances)


net.biota <- graph_from_data_frame(d=biota.link, vertices=biota.node, directed=T)
net.biota

plot(net.biota, vertex.label=NA)

net.biota <- simplify(net.biota, remove.multiple = F, remove.loops = T) 

plot(net.biota, edge.arrow.size=.4,vertex.label=NA, edge.curved=.1)

corls.biota <- c("springgreen4","khaki4",
                 "yellowgreen","tan1","sienna3","brown")
V(net.biota)$color <- corls.biota[V(net.biota)$type]


deg.biota <- degree(net.biota, mode="all")
V(net.biota)$size <- deg.biota*3
plot(net.biota) 


V(net.biota)$size <- V(net.biota)$Abundances_log*8
plot(net.biota) 


V(net.biota)$label <- NA

E(net.biota)$width <- E(net.biota)$estimate*10
plot(net.biota) 

E(net.biota)$arrow.size <- 2
E(net.biota)$edge.color <- "gray80"

graph_attr(net.biota, "layout") <- layout_in_circle
plot(net.biota, edge.curved=.1) 


tiff("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/02_figuras/network/log_network_sig_lf_closed_29-04-20.tif", width = 15, height = 15, units = 'in', res = 500)

plot(net.biota, edge.curved=.1, vertex.color="gray90", vertex.frame.color	= c("brown","sienna3",
                                                                              "khaki4","yellowgreen","tan1","springgreen4"))  


dev.off()


