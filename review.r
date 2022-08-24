# Raster Review
# 4/7/2022 GeoSpatial with R Carpentry


# We load 1-band raster files (often TIFs or JPG's or JP2's)
# into raster objects.

# we convert raster objects 
# into to dataframes for ggplot mapping

# we can map by custom bins

# we can map more than one df at a time

# we can do math on our rasters

# Projections are important

# For color (and beyond!), we map using rasterstack, or (newer) rasterbrick


##############################
# the libraries we need

library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tidyverse)

###############################
# We load 1-band raster files (often TIFs or JPG's or JP2's)
# into raster objects.

# raster files come with metadata attached
GDALinfo("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

SURFACE <- raster(
  "data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

TERRAIN <- raster(
  "data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_dtmCrop.tif")


# we convert raster objects 
# into to dataframes for ggplot mapping

SURFACE_df <- as.data.frame(SURFACE, xy=TRUE)
TERRAIN_df <- as.data.frame(SURFACE, xy=TRUE)
str(TERRAIN_df)


# we can look at statistics
summary(TERRAIN)

# but we want a full sample--otherwise we won't
# know the true max and min
summary(TERRAIN, maxsamp = ncell(TERRAIN))

# ??????????????
# there's another way to set those

# single channel plots come out with
# a default colorscheme
# (I'm remembering the column name from summary() above)

ggplot() +
  geom_raster(data = TERRAIN_df, 
              aes(x = x, y = y, 
                  fill = HARV_dsmCrop)) +
  coord_quickmap()

# we like color-blind safe color schemes
# and certain other patterns for elevations
ggplot() +
  geom_raster(data = TERRAIN_df, 
              aes(x = x, y = y, 
                  fill = HARV_dsmCrop)) +
  scale_fill_viridis_c() +
  coord_quickmap()


################################
# 
# episode 2
#
# we can map by custom bins

# mutate that into some nice breaks
# mutate = add a column

# this one lets R choose the breaks
TERRAIN_df <- TERRAIN_df %>%
  mutate(r_elevation_bins = cut(HARV_dsmCrop, breaks = 3))

# this one makes nice round breaks
# nice round values
summary(SURFACE_df)
custom_bins <- c(300, 310, 325, 335, 350, 360, 375, 400, 450)
custom_bins
str(custom_bins)
TERRAIN_df <- TERRAIN_df %>%
  mutate(my_elevation_bins = cut(HARV_dsmCrop, breaks = custom_bins))

ggplot() +
  geom_bar(data = TERRAIN_df, aes(my_elevation_bins))

# now we can map by my custom bins
ggplot() +
  geom_raster(data = TERRAIN_df , 
              aes(x = x, y = y, fill = my_elevation_bins)) + 
  scale_fill_viridis_d() +
  coord_quickmap()

################################
# we can map more than one df at a time
#    often we will make a pretty hillshade
#    

# order matters
ggplot() +
  geom_raster(data = SURFACE_df,
              aes(x=x, y=y, alpha = HARV_dsmCrop)) +
  scale_alpha(range = c(0.15, 0.85)) +
  scale_fill_viridis_d() +
  geom_raster(data = TERRAIN_df , 
              aes(x = x, y = y, fill = my_elevation_bins)) + 
  coord_quickmap()

ggplot() +
  geom_raster(data = TERRAIN_df , 
              aes(x = x, y = y, fill = my_elevation_bins)) + 
  scale_fill_viridis_d() +
  geom_raster(data = SURFACE_df,
              aes(x=x, y=y, alpha = HARV_dsmCrop)) +
  scale_alpha(range = c(0.15, 0.85)) +
  coord_quickmap()



# Projections are important

# we can do math on our rasters


# For color (and beyond!), we map using rasterstack, or (newer) rasterbrick
