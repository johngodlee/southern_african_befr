# Get Aridity Index values for SEOSAW plots
# John Godlee (johngodlee@gmail.com)
# 2018_12_10
# 2019_09_17

# Preamble ----

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(rgdal)
library(raster)
library(maps)
library(ggplot2)
library(dplyr)

# Import data ----

# Aridity Index
ai <- readGDAL("data/ai_et0_crop/ai_et0_crop.tif")

# SEOSAW plot locations
load("data/seosaw_plot_summary5Apr2019.Rdata")

# Convert plot locations to spatial ----
plot_xy <- data.frame(ssaw8$plotInfoFull$longitude_of_centre, 
	ssaw8$plotInfoFull$latitude_of_centre)

plot_loc_spdf <- SpatialPointsDataFrame(coords = plot_xy, 
  data = as.data.frame(ssaw8$plotInfoFull),
	proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Convert aridity index to raster ----
ai_raster <- raster(ai)

# Get aridity index for each plot ----
ai_seosaw <- extract(ai_raster, plot_loc_spdf)

ssaw8$plotInfoFull$ai <- ai_seosaw

# Export data ----

# Select relevant columns
seosaw_ai <- ssaw8$plotInfoFull %>%
	dplyr::select(plotcode, longitude_of_centre, latitude_of_centre, ai)

# Export
write.csv(seosaw_ai, file = "data/seosaw_aridity_index.csv", row.names = FALSE)

