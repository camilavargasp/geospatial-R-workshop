# install test

library(tidyverse)
library(rgdal)
library(RColorBrewer)
library(sf)
library(raster)

# still need a polygon!!!!

# everything should be relative to "here"
setwd(".")
buildings <- st_read("data/Campus_Buildings.shp")
signs <- st_read("data/Inventory.shp")

campus_DEM <- raster("data/WestCampusDEM.tif")
campus_DEM_df <- as.data.frame(campus_DEM, xy = TRUE)

# geom_sf(data = habitat, size = 0.1, color = 'black', fill = "cyan1")

# 9 signs need to be fixed.
unique(signs$Condition)
fix_me <- signs %>% 
  filter(Condition == "Poor")
nrow(fix_me)

#  color by attribute
ggplot () +
  geom_raster(data = campus_DEM_df, aes(x=x, y=y, fill = WestCampusDEM)) +
  scale_fill_gradient2(na.value = "lightgrey", 
                       low="black", 
                       mid="azure1", 
                       high="cornsilk3",
                       midpoint=0) +
    geom_sf(data = signs, aes(color = factor(Condition)), size = 1.5) +
  labs(color = 'Condition') +
  coord_sf()

