### BILL330
### Professor Mauro Galetti 
### Class #02

# Whittaker biomes _ Exercises #

# -----------------------------------------------------------------------

#First, you should install and/or load some R packages

install.packages("devtools")

devtools::install_github("valentinitnelav/plotbiomes")

install.packages("tidyverse")
library(tidyverse)

install.packages("plotbiomes")
library(plotbiomes)

install.packages("raster")
library(raster)

install.packages("maptools")
library(maptools)

install.packages("ggrepel")
library(ggrepel)

#Setting the directory in your computer

setwd("D:/Google Drive/7. MIAMI/Spring_2020_Galetti/Classes/2020/Day 2_Whittaker_E01/E1_Wittaker biomes")

getwd()


# ------------------------------------------------------------------------

#EXAMPLE:

# ===== Prepare raster stack

# Read temperature and precipitation as raster stack.
# Low resolution raster datasets come with 'plotbiomes' package.

path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
temp_pp <- raster::stack(path)
names(temp_pp) <- c("temperature", "precipitation")

# ===== Generate random locations
data(wrld_simpl) # load world polygons from maptools

# Eliminate Antarctica - it is not represented in the raster datasets.
wrld_simpl <- wrld_simpl[wrld_simpl$NAME != "Antarctica", ]

# Create random locations within world's polygons.
set.seed(66) # random number generator
points <- sp::spsample(x = wrld_simpl, n = 50, type = "random")


# ===== Extract from raster stack

# Extract temperature and precipitation values from the raster datasets
extractions <- raster::extract(temp_pp, points, df = TRUE)

# Adjust temperature values to normal scale because WorldClim temperature data has a scale factor of 10 (integer storage for saving space).
extractions$temperature <- extractions$temperature/10

# Convert precipitation from mm to cm
extractions$precipitation <- extractions$precipitation/10

plot(temp_pp[[1]]/10); points(points)
plot(temp_pp[[2]]); points(points)


whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = extractions, 
             aes(x = temperature, 
                 y = precipitation), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()

my_outliers <- get_outliers(tp = extractions[, 2:3])
extractions$status <- ifelse(extractions$ID %in% my_outliers$row_idx, "out", "in")


whittaker_base_plot() +
  geom_point(data = extractions,
             aes(x = temperature,
                 y = precipitation,
                 colour = status), # map `status` variable to colour
             shape = 16,
             size  = 3) +
  theme_bw()


#saving your data

saveRDS(Whittaker_biomes, "Whittaker_biomes")
my_data2 <- readRDS("Whittaker_biomes")

write.table("my_data2", file="my_data3.xlx", sep=",")


# -----------------------------------------------------------------------

#Exercise 

#Now, we will use data of temperature (Celsius) and rainfall (mm) from 20 different cities around the world, comparing
# 100 years of data (1910 to 2010).

#Open the "01_exercise_Whittaker_LL.csv" file.

cities_world <- read_csv2("C:/Users/Layu/Downloads/E1_Wittaker biomes-20191210T161647Z-001/E1_Wittaker biomes/01_exercise_Whittaker_LL.csv")
cities_world

cities_world$Year <- as.factor(cities_world$Year)
glimpse(cities_world)


#Whittaker plot

whittaker_base_plot() +
  geom_point(data = cities_world,
             aes(x = temperature,
                 y = rainfall,
                 colour = Year), # map `status` variable to colour
             shape = 16,
             size  = 3) +
  scale_color_manual(values = c("red", "blue")) + 
  ggrepel::geom_label_repel(data = cities_world, mapping = aes(x = temperature, y = rainfall, label = FID)) +
  theme_bw()


# end ---------------------------------------------------------------------



