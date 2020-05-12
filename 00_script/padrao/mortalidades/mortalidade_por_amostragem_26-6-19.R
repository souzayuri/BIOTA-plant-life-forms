### YURI - 26-6-19
### Script para mostrar quantas quantas morreram em cada amostragem, ou seja,
### quantas não passaram para a amostragem seguinte.


library(tidyverse)
library(beepr)

ttlplt <- read_csv("Life_Form_yuri_13-1-19_v2na.csv")
ttlplt <- ttlplt[,-c(1,3,5,21:27)]
ttlplt


### T0
gffreqc0 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p0`, `p06` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "0")
gffreqc0

gffreqo0 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p0`, `p06` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "0")
gffreqo0


### T06
gffreqc06 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p06`, `p12` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "06")
gffreqc06

gffreqo06 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p06`, `p12` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "06")
gffreqo06


### T12
gffreqc12 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p12`, `p18` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "12")
gffreqc12

gffreqo12 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p12`, `p18` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "12")
gffreqo12


### T18
gffreqc18 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p18`, `p24` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "18")
gffreqc18

gffreqo18 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p18`, `p24` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "18")
gffreqo18


### T24
gffreqc24 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p24`, `p30` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "24")
gffreqc24

gffreqo24 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p24`, `p30` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "24")
gffreqo24


### T30
gffreqc30 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p30`, `p36` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "30")
gffreqc30

gffreqo30 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p30`, `p36` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "30")
gffreqo30


### T36
gffreqc36 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p36`, `p42` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "36")
gffreqc36

gffreqo36 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p36`, `p42` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "36")
gffreqo36


### T42
gffreqc42 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p42`, `p48` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "42")
gffreqc42

gffreqo42 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p42`, `p48` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "42")
gffreqo42


### T48
gffreqc48 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p48`, `p54` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "48")
gffreqc48

gffreqo48 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p48`, `p54` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "48")
gffreqo48


### T54
gffreqc54 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p54`, `p60` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "54")
gffreqc54

gffreqo54 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p54`, `p60` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "54")
gffreqo54



### T60
gffreqc60 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p60`, `p66` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "60")
gffreqc60

gffreqo60 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p60`, `p66` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "60")
gffreqo60


### T66
gffreqc66 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p66`, `p72` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "66")
gffreqc66

gffreqo66 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p66`, `p72` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "66")
gffreqo66


### T72
gffreqc72 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p72`, `p78` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "72")
gffreqc72

gffreqo72 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p72`, `p78` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "72")
gffreqo72



### T78
gffreqc78 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p78`, `p84` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "78")
gffreqc78

gffreqo78 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p78`, `p84` == 0) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "78")
gffreqo78


### T84
gffreqc84 <- ttlplt  %>% 
  filter(`Treatment` == "close", `p84` == 1) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "close") %>% 
  mutate(Period = "84")
gffreqc84

gffreqo84 <- ttlplt  %>% 
  filter(`Treatment` == "open", `p0` == 1, `p84` == 1) %>% 
  group_by(Site) %>% 
  summarise(Freq = n()) %>% 
  mutate(Tratment = "open") %>% 
  mutate(Period = "84")
gffreqo84

freoccur <- bind_rows(gffreqc0,gffreqo0,gffreqc06,gffreqo06,gffreqc12,gffreqo12,
          gffreqc18,gffreqo18,gffreqc24,gffreqo24,gffreqc30,gffreqo30,
          gffreqc36,gffreqo36,gffreqc42,gffreqo42,gffreqc48,gffreqo48,
          gffreqc54,gffreqo54,gffreqc60,gffreqo60,gffreqc66,gffreqo66,
          gffreqc72,gffreqo72,gffreqc78,gffreqo78,gffreqc84,gffreqo84)

freoccur

write.csv(freoccur, "mortalidade_por_amostragem_26-06-19.csv", row.names = FALSE, quote = TRUE) #row names cria uma coluna com o level


### Não da certo pq não tem CBO nas amostragens 48,54 e 60
#time <- factor(rep(c(0,06,12,18,24,30,36,42,48,54,60,66,72,78,84), each = 8))
#time
#freoccur$time <- factor(rep(c(0,06,12,18,24,30,36,42,48,54,60,66,72,78,84), each = 8))
