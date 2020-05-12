#### Yuri - 24-06-19
#### script para grafico bloxplot da quantidade de meses que as plantulas duram nas parcelas

library(tidyverse)

ttlplt <- read_csv("Total_box_plot_persistencia_ano_tratamento.csv")


par(mar=c(3,4,3,1))
myplot=boxplot(seedling ~ time , ttlplt, boxwex=0.5 , ylab="Seedlings",
               main="Seedlings by Month" , col=c("slateblue1" , "tomato"))

for(i in seq(0.5 , 32 , 2)){ abline(v=i,lty=1, col="grey")}

legend("bottomright", legend = c("Close", "Open"), col=c("slateblue1" , "tomato"),
       pch = 15, bty = "n", pt.cex = 4, cex = 1.3,  horiz = F, inset = c(0.1, 0.8))






### grafico mensal pelo ggplot versao 1

ggplot(ttlplt, aes(x=time, y=seedling, fill = treatment)) + 
  geom_boxplot(width = 0.65) + geom_hline(yintercept = median(ttlplt$seedling, na.rm = T), lty = 5, col = "blue", size = 0.8) +
  scale_fill_manual(name = "Treatment", labels = c("Close", "Open"), values=c("slateblue1","tomato")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
        )  + expand_limits(y=400) + scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400)) +
  labs(x = "Time (month)", y = "Seedlings")
  
  
st
  
  

### grafico mensal pelo ggplot versao 2

ggplot(ttlplt, aes(x=time, y=seedling, fill = treatment)) + 
  geom_boxplot(
    # custom boxes
    alpha=0.7,
    # Notch?
    notch=FALSE,
    notchwidth = 0.1,
    # custom outliers
    outlier.colour="red4",
    outlier.fill="black",
    outlier.size=3
  ) + 
  scale_fill_manual(name = "Treatment", labels = c("Close", "Open"), values=c("slateblue1","tomato")) +  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        panel.border = element_rect(linetype = "solid", size = 1.5, color = "black"),
        panel.grid.major.x = element_line(color = "gray", size = 0.8),
        panel.grid.major.y = element_line(color = "white"),
        #panel.grid.minor.y = element_line(color = "white"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
  )  +
  labs(x = "Time (month)", y = "Seedlings")





### grafico de comparação primeira versao

ggplot(ttlplt, aes(x=treatment, y=seedling, fill = treatment)) +
  geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.2) +
  scale_fill_manual(values=c("slateblue1","tomato")) +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=14,face="bold"),
                     panel.border = element_rect(linetype = "solid", size = 1.5),
                     legend.position = "none") + scale_x_discrete(breaks=c("close", "open"),
                                                                  labels=c("Close", "Open")) +
  labs(x = "Treatment", y = "Seedlings")






### grafico de comparação segunda versao

ggplot(ttlplt, aes(x=treatment, y=seedling, fill = treatment)) +
  geom_boxplot(
    # custom boxes
    alpha=0.7,
    # Notch?
    notch=TRUE,
    notchwidth = 0.1,
    # custom outliers
    outlier.colour="red4",
    outlier.fill="black",
    outlier.size=3
  ) + 
  scale_fill_manual(values=c("slateblue1","tomato")) +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=14,face="bold"),
                     panel.border = element_rect(linetype = "solid", size = 1.5),
                     legend.position = "none") + scale_x_discrete(breaks=c("close", "open"),
                                                                labels=c("Close", "Open")) +
  labs(x = "Treatment", y = "Seedlings")



### Anova e test-t


ajuste <- lm(ttlplt$seedling~ttlplt$treatment)
ajuste
summary(ajuste)
anova(ajuste)
summary(aov(ttlplt$seedling~ttlplt$treatment))
t.test(ttlplt$seedling~ttlplt$treatment)

