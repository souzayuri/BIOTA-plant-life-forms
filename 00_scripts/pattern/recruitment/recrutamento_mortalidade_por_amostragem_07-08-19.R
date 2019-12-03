### YURI - 07-08-19
### Script para mostrar qual o recrutamento em cada amostragem dos individuos que só duraram uma medidação


library(tidyverse)
library(beepr)

setwd("C:/Users/Yuri/Google Drive/Yuri/Mestrado/parcelas_biota/projeto/PPG/tese/analises/01_dados/padrao/")

ttlplt <- read_csv("life_form_yuri_2019v2.csv")
ttlplt[,23:24] <- lapply(ttlplt[,23:24], as.numeric) #transforma as colunas 23 e 24 em numerico
ttlplt
ttlplt <- ttlplt[,-c(1,3,5,25:29)]
str(ttlplt)


### T0
gffreqc0 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p0` == 1, `p06` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "0")
gffreqc0

gffreqo0 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p0` == 1, `p06` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "0")
gffreqo0


### T06
gffreqc06 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p0` == 0, `p06` == 1, `p12` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "06")
gffreqc06

gffreqo06 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p0` == 0, `p06` == 1, `p12` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "06")
gffreqo06


### T12
gffreqc12 <- ttlplt  %>% 
  filter(`Treatment` == "closed",`p06` == 0, `p12` == 1, `p18` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "12")
gffreqc12

gffreqo12 <- ttlplt  %>% 
  filter(`Treatment` == "open",`p06` == 0, `p12` == 1, `p18` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "12")
gffreqo12


### T18
gffreqc18 <- ttlplt  %>% 
  filter(`Treatment` == "closed",`p12` == 0, `p18` == 1, `p24` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "18")
gffreqc18

gffreqo18 <- ttlplt  %>% 
  filter(`Treatment` == "open",`p12` == 0, `p18` == 1, `p24` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "18")
gffreqo18


### T24
gffreqc24 <- ttlplt  %>% 
  filter(`Treatment` == "closed",`p18` == 0, `p24` == 1, `p30` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "24")
gffreqc24

gffreqo24 <- ttlplt  %>% 
  filter(`Treatment` == "open",`p18` == 0, `p24` == 1, `p30` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "24")
gffreqo24


### T30
gffreqc30 <- ttlplt  %>% 
  filter(`Treatment` == "closed",`p24` == 0, `p30` == 1, `p36` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "30")
gffreqc30

gffreqo30 <- ttlplt  %>% 
  filter(`Treatment` == "open",`p24` == 0, `p30` == 1, `p36` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "30")
gffreqo30


### T36
gffreqc36 <- ttlplt  %>% 
  filter(`Treatment` == "closed",`p30` == 0, `p36` == 1, `p42` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "36")
gffreqc36

gffreqo36 <- ttlplt  %>% 
  filter(`Treatment` == "open",`p30` == 0, `p36` == 1, `p42` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "36")
gffreqo36


### T42
gffreqc42 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p36` == 0, `p42` == 1, `p48` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "42")
gffreqc42

gffreqo42 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p36` == 0, `p42` == 1, `p48` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "42")
gffreqo42


### T48
gffreqc48 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p42` == 0, `p48` == 1, `p54` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "48")
gffreqc48

gffreqo48 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p42` == 0, `p48` == 1, `p54` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "48")
gffreqo48


### T54
gffreqc54 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p48` == 0, `p54` == 1, `p60` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "54")
gffreqc54

gffreqo54 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p48` == 0, `p54` == 1, `p60` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "54")
gffreqo54



### T60
gffreqc60 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p54` == 0, `p60` == 1, `p66` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "60")
gffreqc60

gffreqo60 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p54` == 0, `p60` == 1, `p66` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "60")
gffreqo60


### T66
gffreqc66 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p60` == 0, `p66` == 1, `p72` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "66")
gffreqc66

gffreqo66 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p60` == 0, `p66` == 1, `p72` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "66")
gffreqo66


### T72
gffreqc72 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p66` == 0, `p72` == 1, `p78` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "72")
gffreqc72

gffreqo72 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p66` == 0, `p72` == 1, `p78` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "72")
gffreqo72



### T78
gffreqc78 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p72` == 0, `p78` == 1, `p84` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "78")
gffreqc78

gffreqo78 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p72` == 0, `p78` == 1, `p84` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "78")
gffreqo78



### T84
gffreqc84 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p78` == 0, `p84` == 1, `p90` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "84")
gffreqc84

gffreqo84 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p78` == 0, `p84` == 1, `p90` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "84")
gffreqo84



### T90
gffreqc90 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p84` == 0, `p90` == 1, `p96` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "90")
gffreqc90

gffreqo90 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p84` == 0, `p90` == 1, `p96` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "90")
gffreqo90


### T96
gffreqc96 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p90` == 0, `p96` == 1, `p102` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "96")
gffreqc96

gffreqo96 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p90` == 0, `p96` == 1, `p102` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "96")
gffreqo96


### T102
gffreqc102 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p96` == 0, `p102` == 1, `p108` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "102")
gffreqc102

gffreqo102 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p96` == 0, `p102` == 1, `p108` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "102")
gffreqo102


### T108
gffreqc108 <- ttlplt  %>% 
  filter(`Treatment` == "closed", `p102` == 0, `p108` == 1) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "closed") %>% 
  mutate(Period = "108")
gffreqc108

gffreqo108 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p102` == 0, `p108` == 1) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Treatment = "open") %>% 
  mutate(Period = "108")
gffreqo108


frerecruit <- bind_rows(gffreqc0,gffreqo0,gffreqc06,gffreqo06,gffreqc12,gffreqo12,
                      gffreqc18,gffreqo18,gffreqc24,gffreqo24,gffreqc30,gffreqo30,
                      gffreqc36,gffreqo36,gffreqc42,gffreqo42,gffreqc48,gffreqo48,
                      gffreqc54,gffreqo54,gffreqc60,gffreqo60,gffreqc66,gffreqo66,
                      gffreqc72,gffreqo72,gffreqc78,gffreqo78,gffreqc84,gffreqo84,
                      gffreqc90,gffreqo90,gffreqc96,gffreqo96,gffreqc102,gffreqo102,
                      gffreqc108,gffreqo108)

frerecruit

write_csv(frerecruit, "recrut_mort_por_amostragem_07-08-19.csv") #row names cria uma coluna com o level


### Não da certo pq não tem CBO nas amostragens 48,54 e 60
#time <- factor(rep(c(0,06,12,18,24,30,36,42,48,54,60,66,72,78,84), each = 8))
#time
#freoccur$time <- factor(rep(c(0,06,12,18,24,30,36,42,48,54,60,66,72,78,84), each = 8))
