
# Script para calcular abundancia das formas de vida a partir da matriz de presença e ausencia no tempo 

library("tidyverse")
library("readxl")
library("writexl")
library("ggplot2")
library("reshape")
library("tidyquant")  # Loads tidyverse, tidquant, financial pkgs, xts/zoo
library("cranlogs")   # For inspecting package downloads over year
library("survival")
library("vegan")
library("beepr")

extractsum <- as.data.frame(read_csv("Life_Form_yuri_13-1-19.csv"))


# ITA CLOSED 1
CBOC841 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 1 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC841

CBOC781 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 1 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC781

ITA1 = (CBOC841+CBOC781)
ITA1


plot1 <- c("Plot1")
loc <- c(ITA1)
df1 <- data.frame(plot1, loc)
df1



# ITA CLOSED 2
CBOC842 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 2 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC842

CBOC782 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 2 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC782

ITA2 = (CBOC842+CBOC782)
ITA2

plot2 <- c("Plot2")
loc <- c(ITA2)
df2 <- data.frame(plot2, loc)
df2

# ITA CLOSED 3
CBOC843 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 3 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC843

CBOC783 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 3 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC783

ITA3 = (CBOC843+CBOC783)
ITA3

plot3 <- c("Plot3")
loc <- c(ITA3)
df3 <- data.frame(plot3, loc)
df3


# ITA CLOSED 4
CBOC844 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 4 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC844

CBOC784 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 4 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC784

ITA4 = (CBOC844+CBOC784)
ITA4

plot4 <- c("Plot4")
loc <- c(ITA4)
df4 <- data.frame(plot4, loc)
df4


# ITA CLOSED 5
CBOC845 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 5 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC845

CBOC785 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 5 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC785

ITA5 = (CBOC845+CBOC785)
ITA5

plot5 <- c("Plot5")
loc <- c(ITA5)
df5 <- data.frame(plot5, loc)
df5



# ITA CLOSED 6
CBOC846 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 6 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC846

CBOC786 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 6 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC786

ITA6 = (CBOC846+CBOC786)
ITA6

plot6 <- c("Plot6")
loc <- c(ITA6)
df6 <- data.frame(plot6, loc)
df6


# ITA CLOSED 7
CBOC847 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 7 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC847

CBOC787 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 7 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC787

ITA7 = (CBOC847+CBOC787)
ITA7

plot7 <- c("Plot7")
loc <- c(ITA7)
df7 <- data.frame(plot7, loc)
df7


# ITA CLOSED 8
CBOC848 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 8 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC848

CBOC788 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 8 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC788

ITA8 = (CBOC848+CBOC788)
ITA8

plot8 <- c("Plot8")
loc <- c(ITA8)
df8 <- data.frame(plot8, loc)
df8


# ITA CLOSED 9
CBOC849 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 9 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC849

CBOC789 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 9 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC789

ITA9 = (CBOC849+CBOC789)
ITA9

plot9 <- c("Plot9")
loc <- c(ITA9)
df9 <- data.frame(plot9, loc)
df9


# ITA CLOSED 10
CBOC8410 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 10 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC8410

CBOC7810 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 10 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC7810

ITA10 = (CBOC8410+CBOC7810)
ITA10

plot10 <- c("Plot10")
loc <- c(ITA10)
df10 <- data.frame(plot10, loc)
df10

# ITA CLOSED 11
CBOC8411 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 11 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC8411

CBOC7811 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 11 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC7811

ITA11 = (CBOC8411+CBOC7811)
ITA11

plot11 <- c("Plot11")
loc <- c(ITA11)
df11 <- data.frame(plot11, loc)
df11


# ITA CLOSED 12
CBOC8412 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 12 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC8412

CBOC7812 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 12 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC7812

ITA12 = (CBOC8412+CBOC7812)
ITA12

plot12 <- c("Plot12")
loc <- c(ITA12)
df12 <- data.frame(plot12, loc)
df12



# ITA CLOSED 13
CBOC8413 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 13 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC8413

CBOC7813 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 13 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC7813

ITA13 = (CBOC8413+CBOC7813)
ITA13

plot13 <- c("Plot13")
loc <- c(ITA13)
df13 <- data.frame(plot13, loc)
df13



# ITA CLOSED 14
CBOC8414 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 14 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC8414

CBOC7814 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 14 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC7814

ITA14 = (CBOC8414+CBOC7814)
ITA14

plot14 <- c("Plot14")
loc <- c(ITA14)
df14 <- data.frame(plot14, loc)
df14


# ITA CLOSED 15
CBOC8415 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 15 & `Treatment` == "close"	 & `Trecruit` == "T84" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC8415

CBOC7815 <- extractsum  %>% 
  filter( Site == "CBO" & `Plot` == 15 & `Treatment` == "close"	 & `Trecruit` == "T78" & `Life_Form` == "herb" ) %>%  
  summarise(abundance = length(Life_Form))
CBOC7815

ITA15 = (CBOC8415+CBOC7815)
ITA15

plot15 <- c("Plot15")
loc <- c(ITA15)
df15 <- data.frame(plot15, loc)
df15



#ITA LISTA

CBOC <- data.frame(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15)
CBOC


plots <- CBOC[ ,c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)]
plots

abundance <- CBOC[ ,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)]
abundance


abunCBOC <- abundance %>% 
  # ele coleta as colunas de t1:t17 e as agregas na coluna "time"
  gather(key = "plot", value = "value", 1:15)

abunCBOC



plotCBOC <- plots %>% 
  # ele coleta as colunas de t1:t17 e as agregas na coluna "time"
  gather(key = "plot", value = "plot", 1:15)
plotCBOC

dfCBOC <- data.frame(plotCBOC, abunCBOC)
dfCBOC

dfCBOC <- dfCBOC[,-c(2:3)]
dfCBOC

#beep(sound = 1, expr = NULL)
