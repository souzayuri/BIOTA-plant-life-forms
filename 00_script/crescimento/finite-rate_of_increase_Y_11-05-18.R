
# Informations ------------------------------------------------------------

### title: finite rate of increase ###
### author: Yuri (yuri.eco2013@gmail.com)
### data: 11/05/2019
### Description: This script creates a growth rate by each sampled.




# Load packages and set directory -------------------------------------

rm(list = ls())

if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("textclean")) install.packages("textclean", dependencies = TRUE)

path <- ("C:/Users/Layu/Downloads/01_dados/padrao/")
setwd(path)




# load data table ----------------------------------------------------------

data_biota <- read_csv("life_form_yuri_2019v2.csv", locale = locale(encoding = "ASCII"))
glimpse(data_biota)

data_biota$Individual <- str_remove_all(data_biota$Individual, "[1234567890]")

data.biota.den <- data_biota %>%
  dplyr::select(-c(1,23:26,28)) %>%   # remove unused columns
  gather(key = "Month", value = "value", 5:21) %>% 
  rename(life_form = `Life Form`) %>% # convert the information from columns 4 to 20 in the columns "time" (month data) and column "value", that contains the record of each individual 
  mutate(time = Month %>% stringr::str_replace("p", "")) %>% 
  mutate(Individual = Individual %>% str_remove_all("[1234567890]")) %>% 
  textclean::drop_row("life_form", c("indeterminate")) %>%
  dplyr::group_by(Site, Plot, Treatment, Month, time, life_form, Species) %>% 
  summarise(abundances = sum(value))

data.biota.den


# euterpe -----------------------------------------------------------------

euterpe <- data.biota.den %>% 
  filter(`Species` == "Euterpe edulis") %>% 
  group_by(Site, Plot, time, Treatment) %>% 
  summarise(abundances_euterpe = sum(abundances))
euterpe



# euterpe t0 --------------------------------------------------------------

lambda.euterpe.t0 <- euterpe %>% 
  group_by(Site, Plot, time, Treatment, abundances_euterpe) %>% 
  filter(`time` == "0") %>% 
  summarise(lambda_euterpe = abundances_euterpe/abundances_euterpe)
lambda.euterpe.t0


# euterpe t06 -------------------------------------------------------------

lambda.euterpe.t06 <- euterpe %>% 
  filter(`time` == "06") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t06


lambda.euterpe.t06["lambda_euterpe"] <- lambda.euterpe.t06$abundances_euterpe/lambda.euterpe.t0$abundances_euterpe
lambda.euterpe.t06


# euterte t12 -------------------------------------------------------------

lambda.euterpe.t12 <- euterpe %>% 
  filter(`time` == "12") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t12


lambda.euterpe.t12["lambda_euterpe"] <- lambda.euterpe.t12$abundances_euterpe/lambda.euterpe.t06$abundances_euterpe
lambda.euterpe.t12



# euterpe t18 -------------------------------------------------------------
lambda.euterpe.t18 <- euterpe %>% 
  filter(`time` == "18") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t18

lambda.euterpe.t18["lambda_euterpe"] <- lambda.euterpe.t18$abundances_euterpe/lambda.euterpe.t12$abundances_euterpe
lambda.euterpe.t18



# euterpe t24 -------------------------------------------------------------
lambda.euterpe.t24 <- euterpe %>% 
  filter(`time` == "24") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t24

lambda.euterpe.t24["lambda_euterpe"] <- lambda.euterpe.t24$abundances_euterpe/lambda.euterpe.t18$abundances_euterpe
lambda.euterpe.t24




# euterpe t30 -------------------------------------------------------------


lambda.euterpe.t30 <- euterpe %>% 
  filter(`time` == "30") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t30


lambda.euterpe.t30["lambda_euterpe"] <- lambda.euterpe.t30$abundances_euterpe/lambda.euterpe.t24$abundances_euterpe
lambda.euterpe.t30



# euterpe t36 -------------------------------------------------------------

lambda.euterpe.t36 <- euterpe %>% 
  filter(`time` == "36") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t36


lambda.euterpe.t36["lambda_euterpe"] <- lambda.euterpe.t36$abundances_euterpe/lambda.euterpe.t30$abundances_euterpe
lambda.euterpe.t36


# euterpe t42 -------------------------------------------------------------


lambda.euterpe.t42 <- euterpe %>% 
  filter(`time` == "42") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t42


lambda.euterpe.t42["lambda_euterpe"] <- lambda.euterpe.t42$abundances_euterpe/lambda.euterpe.t36$abundances_euterpe
lambda.euterpe.t42


# euterpe t48 -------------------------------------------------------------


lambda.euterpe.t48 <- euterpe %>% 
  filter(`time` == "48") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t48


lambda.euterpe.t48["lambda_euterpe"] <- lambda.euterpe.t48$abundances_euterpe/lambda.euterpe.t42$abundances_euterpe
lambda.euterpe.t48

# euterpe t54 -------------------------------------------------------------


lambda.euterpe.t54 <- euterpe %>% 
  filter(`time` == "54") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t54


lambda.euterpe.t54["lambda_euterpe"] <- lambda.euterpe.t54$abundances_euterpe/lambda.euterpe.t48$abundances_euterpe
lambda.euterpe.t54

# euterpe t60 -------------------------------------------------------------


lambda.euterpe.t60 <- euterpe %>% 
  filter(`time` == "60") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t60


lambda.euterpe.t60["lambda_euterpe"] <- lambda.euterpe.t60$abundances_euterpe/lambda.euterpe.t54$abundances_euterpe
lambda.euterpe.t60


# euterpe t66 -------------------------------------------------------------


lambda.euterpe.t66 <- euterpe %>% 
  filter(`time` == "66") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t66


lambda.euterpe.t66["lambda_euterpe"] <- lambda.euterpe.t66$abundances_euterpe/lambda.euterpe.t60$abundances_euterpe
lambda.euterpe.t66

# euterpe t72 -------------------------------------------------------------


lambda.euterpe.t72 <- euterpe %>% 
  filter(`time` == "72") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t72


lambda.euterpe.t72["lambda_euterpe"] <- lambda.euterpe.t72$abundances_euterpe/lambda.euterpe.t66$abundances_euterpe
lambda.euterpe.t72


# euterpe t78 -------------------------------------------------------------


lambda.euterpe.t78 <- euterpe %>% 
  filter(`time` == "78") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t78


lambda.euterpe.t78["lambda_euterpe"] <- lambda.euterpe.t78$abundances_euterpe/lambda.euterpe.t72$abundances_euterpe
lambda.euterpe.t78


# euterpe t84 -------------------------------------------------------------


lambda.euterpe.t84 <- euterpe %>% 
  filter(`time` == "84") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t84


lambda.euterpe.t84["lambda_euterpe"] <- lambda.euterpe.t84$abundances_euterpe/lambda.euterpe.t78$abundances_euterpe
lambda.euterpe.t84



# euterpe t90 -------------------------------------------------------------


lambda.euterpe.t90 <- euterpe %>% 
  filter(`time` == "90") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t90


lambda.euterpe.t90["lambda_euterpe"] <- lambda.euterpe.t90$abundances_euterpe/lambda.euterpe.t84$abundances_euterpe
lambda.euterpe.t90


# euterpe t96 -------------------------------------------------------------


lambda.euterpe.t96 <- euterpe %>% 
  filter(`time` == "96") %>% 
  group_by(Site, Plot, time, Treatment)
lambda.euterpe.t96


lambda.euterpe.t96["lambda_euterpe"] <- lambda.euterpe.t96$abundances_euterpe/lambda.euterpe.t90$abundances_euterpe
lambda.euterpe.t96



# bind --------------------------------------------------------------------

lambda.euterpe <- bind_rows(lambda.euterpe.t0,lambda.euterpe.t06,lambda.euterpe.t12,
                            lambda.euterpe.t18,lambda.euterpe.t24,lambda.euterpe.t30,
                            lambda.euterpe.t36,lambda.euterpe.t42,lambda.euterpe.t48,
                            lambda.euterpe.t54,lambda.euterpe.t60,lambda.euterpe.t66,
                            lambda.euterpe.t72,lambda.euterpe.t78,lambda.euterpe.t84,
                            lambda.euterpe.t90,lambda.euterpe.t96)
lambda.euterpe$time <- as.numeric(lambda.euterpe$time)



# graphic -----------------------------------------------------------------

tree <- ggplot(lambda.euterpe, aes(time, lambda_euterpe, color = Treatment)) +  
  geom_point() + geom_smooth() + 
tree
