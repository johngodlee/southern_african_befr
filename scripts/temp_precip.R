# Generate dataset of precip and temp means across the miombo ecoregion
# John Godlee (johngodlee@gmail.com)
# 2019-10-02
# 2020-06-22
# 2020-08-19

# Packages
library(raster)
library(sf)
library(nngeo)  # st_remove_holes()
library(dplyr)

# Get Miombo outline ---- 
white_veg <- st_read("/Volumes/john/whitesveg/whitesveg.shp")

# Get southern woodlands only
white_veg_miombo <- white_veg %>%
  filter(DESCRIPTIO %in% c("Moist-infertile savanna")) %>%
  st_make_valid() %>%
  st_crop(., xmin=-180, xmax=180, ymin=-90, ymax=-2) %>%
  `st_crs<-`(4326) %>%
  st_remove_holes()

saveRDS(white_veg_miombo, "data/white_veg_miombo.rds")

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

  # Crop to miombo outline
  allrasters_crop <- crop(allrasters, extent(white_veg_miombo))
  allrasters_mask <- mask(allrasters_crop, white_veg_miombo)

  # Take mean of all in stack
  if (grepl("tavg", x[1])) {
    allrasters_calc <- calc(allrasters_mask, mean, na.rm = TRUE)
  } 
  else if (grepl("prec", x[1])) {
    allrasters_calc <- calc(allrasters_mask, sum, na.rm = TRUE)
  } 

  # Extract all values
  vals <- values(allrasters_calc)

  # Take CV of all values
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
  p_vals = rastlist_extrac[[2]][,1], p_cv = rastlist_extrac[[2]][,2])

# Save to file
saveRDS(t_p, "data/temp_precip.rds")

