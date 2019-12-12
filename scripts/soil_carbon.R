# Extract soil carbon from OCDENS
# John Godlee (johngodlee@gmail.com)
# 2019_09_19

# Preamble ----

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages 
library(rgdal)
library(raster)
library(dplyr)

# Organic Carbon 
ocdens <- readGDAL("data/OCDENS_M_sl2_250m_ll_crop.tif")

# SEOSAW plot locations
load("data/seosaw_plot_summary5Apr2019.Rdata")

# Convert plot locations to spatial ----
plot_xy <- data.frame(ssaw8$plotInfoFull$longitude_of_centre, 
	ssaw8$plotInfoFull$latitude_of_centre)

plot_loc_spdf <- SpatialPointsDataFrame(coords = plot_xy, 
  data = as.data.frame(ssaw8$plotInfoFull),
	proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Convert aridity index to raster ----
ocdens_raster <- raster(ocdens)

# Get aridity index for each plot ----
ocdens_seosaw <- raster::extract(ocdens_raster, plot_loc_spdf)

ssaw8$plotInfoFull$ocdens <- ocdens_seosaw

# Export data ----

# Select relevant columns
seosaw_ocdens <- ssaw8$plotInfoFull %>%
	dplyr::select(plotcode, longitude_of_centre, latitude_of_centre, ocdens)

# Export
saveRDS(seosaw_ocdens, "data/seosaw_ocdens.rds")
