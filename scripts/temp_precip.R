# Generate dataset of precip and temp means across miombo ecoregion
# John Godlee (johngodlee@gmail.com)
# 2019-10-02
# 2020-06-22

# Clean env.
rm(list = ls())

# Packages
library(raster)
library(rgdal)

# Get Miombo outline ---- 
white_veg <- readOGR(dsn="/Volumes/john/whitesveg", layer="")

# Subset to White's miombo 
white_veg_miombo <- white_veg[!is.na(white_veg@data$DESCRIPTIO) & 
  white_veg@data$DESCRIPTIO == "Moist-infertile savanna", ]

# Get southern part only
white_veg_miombo_south <- crop(white_veg_miombo, extent(10, 40, -30, 0))

# Fill holes
white_veg_miombo_south_fill <- SpatialPolygons(list(Polygons(
      Filter(function(f) { 
        f@ringDir==1
      }, white_veg_miombo_south@polygons[[1]]@Polygons), ID = 1)))

# Get Worldclim data ----

# Get list of files
rastlist_t <- list.files(path = "/Volumes/john/worldclim/wc2.1_30s_tavg", 
  pattern = '.tif$', 
  all.files = TRUE, 
  full.names = TRUE)

rastlist_p <- list.files(path = "/Volumes/john/worldclim/wc2.1_30s_prec", 
  pattern = '.tif$', 
  all.files = TRUE, 
  full.names = TRUE)

rastlist <- list(rastlist_t, rastlist_p)

rastlist_extrac <- lapply(rastlist, function(x) {
  # Import rasters
  allrasters <- stack(lapply(x, raster))

  # Crop and mask to miombo outline
  allrasters_crop <- crop(allrasters, extent(white_veg_miombo_south_fill))
  allrasters_mask <- mask(allrasters_crop, white_veg_miombo_south_fill)

  # Take mean of all in stack
  if (grepl("tavg", x[1])) {
    allrasters_calc <- calc(allrasters_mask, mean, na.rm = TRUE)
  } 
  else if (grepl("prec", x[1])) {
    allrasters_calc <- calc(allrasters_mask, sum, na.rm = TRUE)
  } 

  # Extract all values
  vals <- values(allrasters_calc)

  # Take CoV of all values
  allrasters_cv <- calc(allrasters_mask, fun = function(y){
    sd(y, na.rm = TRUE) / mean(y, na.rm = TRUE) * 100
  })

  # Extract all values
  vals_cv <- values(allrasters_cv)

  # Create dataframe
  out <- data.frame(vals, vals_cv)
  return(out)
})

# Combine to dataframe
t_p <- data.frame(t_vals = rastlist_extrac[[1]][,1], t_cv = rastlist_extrac[[1]][,2],
  p_vals = rastlist_extrac[[2]][,1], p_cv = rastlist_extrac[[2]][,2],

# Save to file
saveRDS(t_p, "data/temp_precip.rds")

