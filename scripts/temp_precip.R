# Generate a dataset of precipitation and temperature means across the Miombo region
# John Godlee (johngodlee@gmail.com)
# 2019_10_02

rm(list = ls())

# Packages
library(raster)
library(rgdal)

# Get Miombo shape
white_veg <- readOGR(dsn="data/whitesveg", 
  layer="Whites vegetation")

white_veg_miombo <- white_veg[!is.na(white_veg@data$DESCRIPTIO) & white_veg@data$DESCRIPTIO == "Moist-infertile savanna" , ]

white_veg_miombo_south <- crop(white_veg_miombo, extent(10, 40, -30, 0))

load("data/seosaw_plot_summary5Apr2019.Rdata")

plot_loc <- data.frame(plotcode = ssaw8$plotInfoFull$plotcode,
  lon = ssaw8$plotInfoFull$longitude_of_centre,
  lat = ssaw8$plotInfoFull$latitude_of_centre)

plot_loc_spdf <- SpatialPointsDataFrame(coords = data.frame(plot_loc$lon, plot_loc$lat), data = plot_loc)

# Temperature ----

# Get list of files
rastlist_t <- list.files(path = "data/wc2.0_30s_tavg", 
  pattern = '.tif$', 
  all.files = TRUE, 
  full.names = TRUE)

# Import
allrasters_t <- lapply(rastlist_t, raster)

# Crop to plot extent
allrasters_t_crop <- lapply(allrasters_t, function(x){
  crop(x, plot_loc_spdf)
})

# Stack
allrasters_t_crop_stack <- raster::stack(allrasters_t_crop)

# Take mean of all in stack
allrasters_t_mean <- calc(allrasters_t_crop_stack, mean, na.rm = TRUE)

# Crop to miombo outline
allrasters_t_mean_crop_mask <- mask(allrasters_t_mean, white_veg_miombo_south)

# Extract all values
t_vals <- values(allrasters_t_mean_crop_mask)

# Take CV of all values
allrasters_t_cv <- calc(allrasters_t_crop_stack, fun = function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  })

# Crop to miombo outline
allrasters_t_cv_crop_mask <- mask(allrasters_t_cv, white_veg_miombo_south)

# Extract all values
t_cv <- values(allrasters_t_cv_crop_mask)

# Precipitation ----

# Get list of files
rastlist_p <- list.files(path = "data/wc2.0_30s_prec", 
  pattern = '.tif$', 
  all.files = TRUE, 
  full.names = TRUE)

# Import
allrasters_p <- lapply(rastlist_p, raster)

# Crop to plot extent
allrasters_p_crop <- lapply(allrasters_p, function(x){
  crop(x, plot_loc_spdf)
})

# Stack
allrasters_p_crop_stack <- raster::stack(allrasters_p_crop)

# Take mean of all in stack
allrasters_p_mean <- calc(allrasters_p_crop_stack, sum, na.rm = TRUE)

# Crop to miombo outline
allrasters_p_mean_crop_mask <- mask(allrasters_p_mean, white_veg_miombo_south)

# Extract all values
p_vals <- values(allrasters_p_mean_crop_mask)

# Take CV of all values
allrasters_p_cv <- calc(allrasters_p_crop_stack, fun = function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  })

# Crop to miombo outline
allrasters_p_cv_crop_mask <- mask(allrasters_p_cv, white_veg_miombo_south)

# Extract all values
p_cv <- values(allrasters_p_cv_crop_mask)

# Combine to dataframe
t_p <- data.frame(t_vals, t_cv, p_vals, p_cv)

saveRDS(t_p, "data/temp_precip.rds")

# Get good temperature values for all the plots in SEOSAW, bioclim is wank
t_plot <- extract(allrasters_t_mean, plot_loc_spdf)
t_cv_plot <- extract(allrasters_t_cv, plot_loc_spdf)

plot_temp <- data.frame(plotcode = plot_loc$plotcode, t_plot, t_cv_plot)

saveRDS(plot_temp, "data/plot_temp.rds")

# Get good precip values for all the plots in SEOSAW, bioclim is wank
p_plot <- extract(allrasters_p_mean, plot_loc_spdf)
p_cv_plot <- extract(allrasters_p_cv, plot_loc_spdf)

plot_precip <- data.frame(plotcode = plot_loc$plotcode, p_plot, p_cv_plot)

saveRDS(plot_precip, "data/plot_precip.rds")

