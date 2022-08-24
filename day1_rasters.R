## Carpentry Workshop
## Intro to Geospatial Raster and Vector Data with R 

## Set Up
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)

## View Raster File Attributes

# Raster: n its simplest form, a raster consists of a matrix of cells (or pixels) organized into rows and columns (or a grid) where each cell contains a value representing information, such as temperature. Rasters are digital aerial photographs, imagery from satellites, digital pictures, or even scanned maps.
#Source: https://desktop.arcgis.com/en/arcmap/latest/manage-data/raster-and-images/what-is-raster-data.htm

# Get the specific metadata info for this specific image
GDALinfo("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

# Store the information of the metadata in R. As a vector?? 

HARV_dsmCrop_info <- capture.output(
  GDALinfo("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")
)

## Open a Raster in R

DSM_HARV <- 
  raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

DSM_HARV

summary(DSM_HARV)
# gives me awarning that its calculating the stats from a certain percent of the data

# for a more specific summary
summary(DSM_HARV, maxsamp = ncell(DSM_HARV))

# no warnng in this time!

## Convert raster into a dataframe to plot in ggplot

DSM_HARV_df <- as.data.frame(DSM_HARV, xy = TRUE)

str(DSM_HARV_df)

# plot the raster
ggplot() +
  geom_raster(data = DSM_HARV_df , 
              aes(x = x, y = y, fill = HARV_dsmCrop)) +
  scale_fill_viridis_c() +
  coord_quickmap()

# to easy see a raster we can also use plot
plot(DSM_HARV)

## Coordinate Reference System
# How is the "image" projected?

crs(DSM_HARV)
# finding the min and max value
minValue(DSM_HARV)
maxValue(DSM_HARV)

# If the minimum and maximum values haven’t already been calculated, we can calculate them using the setMinMax() function.
DSM_HARV <- setMinMax(DSM_HARV)

# Plotting an histogram of the values of the raster can help check if the values are within the expected range

DSM_HARV_df %>% 
ggplot()+
  geom_histogram(aes(HARV_dsmCrop), bins = 40)


## Use the output from the GDALinfo() function to find out what NoDataValue is used for our DSM_HARV dataset.

GDALinfo("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

## We cna see that no data value is -9999

GDALinfo("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_DSMhill.tif")


HARV <- 
  raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_DSMhill.tif")

crs(HARV)
nlayers(HARV)
res(HARV)

## Plot rastr Data
DSM_HARV_df <- DSM_HARV_df %>% 
  mutate(fct_elevation = cut(HARV_dsmCrop, breaks = 3)) 

DSM_HARV_df %>%
  ggplot()+
  geom_bar(aes(fct_elevation))

# Documentation about the cut() function: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cut


## Customizinf the bins
custom_bins <- c(300, 350, 400, 450)

DSM_HARV_df <- DSM_HARV_df %>%
  mutate(fct_elevation_2 = cut(HARV_dsmCrop, breaks = custom_bins))

# Tip: Note that when we assign break values a set of 4 values will result in 3 bins of data. The bin intervals are shown using ( to mean exclusive and ] to mean inclusive. For example: (305, 342] means “from 306 through 342”.

ggplot() +
  geom_bar(data = DSM_HARV_df, aes(fct_elevation_2))

# We can use those groups to plot our raster data, with each group being a different color

DSM_HARV_df %>% 
  ggplot()+
  geom_raster(aes(x = x, y = y, fill = fct_elevation_2))+
  coord_quickmap()

## layering rasters
DSM_hill_HARV <-
  raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_DSMhill.tif")

DSM_hill_HARV

DSM_hill_HARV_df <- as.data.frame(DSM_hill_HARV, xy = TRUE)
str(DSM_hill_HARV_df)

# Plot
DSM_hill_HARV_df %>% 
ggplot() +
  geom_raster(aes(x = x, y = y, 
                  # Indicating the transparency of the variable. We can use fill too?
                  alpha = HARV_DSMhill)) + 
  scale_alpha(range =  c(0.15, 0.65), guide = "none") + 
  coord_quickmap()

## Layering rasters
ggplot() +
  geom_raster(data = DSM_HARV_df, 
              aes(x = x, y = y, 
                  fill = HARV_dsmCrop)) + 
  geom_raster(data = DSM_hill_HARV_df, 
              aes(x = x, y = y, 
                  alpha = HARV_DSMhill)) +  
  scale_fill_viridis_c() +  
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()


## Reproject Raster Data

# Bothe files have different coordinate reference system aka projections?
DTM_HARV <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_dtmCrop.tif")

DTM_hill_HARV <- raster("data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_DTMhill_WGS84.tif")


DTM_HARV_df <- as.data.frame(DTM_HARV, xy = TRUE)

DTM_hill_HARV_df <- as.data.frame(DTM_hill_HARV, xy = TRUE)


ggplot() +
  geom_raster(data = DTM_HARV_df , 
              aes(x = x, y = y, 
                  fill = HARV_dtmCrop)) + 
  geom_raster(data = DTM_hill_HARV_df, 
              aes(x = x, y = y, 
                  alpha = HARV_DTMhill_WGS84)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

## Are the CRS of my two rasters the same?
compareCRS(DTM_HARV, DTM_hill_HARV)

# THe CRS is a nested object that lives inside the object. So we can reproject on raster based on the CRS of the other.

DTM_hill_reproj <- projectRaster(DTM_hill_HARV,
crs = crs(DTM_HARV))

DTM_hill_2_df <- as.data.frame(DTM_hill_reproj, xy = TRUE)

## Plot with new projections
ggplot() +
  geom_raster(data = DTM_HARV_df , 
              aes(x = x, y = y, 
                  fill = HARV_dtmCrop)) + 
  geom_raster(data = DTM_hill_2_df, 
              aes(x = x, y = y, 
                  alpha = HARV_DTMhill_WGS84)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()


## Raster Calculation
# Doing math with rasters to get the height on of the canopy.


# Create the DSM plot - Digital Surface Model - Shows elevation data from a terrain

ggplot() +
  geom_raster(data = DSM_HARV_df , 
              aes(x = x, y = y, fill = HARV_dsmCrop)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()

## Create the DTM plot - Digital Terrain Model - shows the map of the terrain, no elevation, just ground or the bottom ofnthe trees.
ggplot() +
  geom_raster(data = DTM_HARV_df , 
              aes(x = x, y = y, fill = HARV_dtmCrop)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()


## Raster math
# CHM - Canopy Height Model - If we subtract the DSM - DTM we get the model of the height of the Canopy.

CHM_HARV <- DSM_HARV - DTM_HARV

CHM_HARV_df <- as.data.frame(CHM_HARV, xy = TRUE)


# Plot CHM
ggplot() +
  geom_raster(data = CHM_HARV_df , 
              aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_quickmap()

## Checj the distribution by plotting a histogram
ggplot(CHM_HARV_df) +
  geom_histogram(aes(layer), bins = 30)


