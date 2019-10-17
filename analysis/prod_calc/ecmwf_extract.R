# Extract data from ECMWF netCDF files for SEOSAW plots
# John Godlee (johngodlee@gmail.com)
# 2018_10_30
# 2019_09_30

# Preamble ----

# Remove old crap
rm(list = ls())

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)
library(ncdf4)
library(parallel)

num_cores <- detectCores()

# Functions ----

# Vapour pressure (millibars)
vp <- function(T_c) {
  return(0.6108 * exp(17.27 * T_c / (T_c + 237.3)))
}

# Relative humidity (%)
rh <- function(T_c, Td_c) {
  es_calc <- vp(T_c)  # Saturation vp
  ea_calc <- vp(Td_c)  # Actual vp
  return((ea_calc / es_calc) * 100)
}

# Vapour pressure deficit (kPa)
vpd <- function(T_c, Td_c) {
  es_calc <- vp(T_c)
  ea_calc <- vp(Td_c)
  return(0.1 * (ea_calc - es_calc))
}

# Extract values from nc_file for a single plot
nc_extract <- function(plot, nc_file){
  lon <- nc_file$dim$longitude$vals
  lat <- nc_file$dim$latitude$vals
  time <- nc_file$dim$time$vals

  lon_match <- plot$longitude_of_centre
  lat_match <- plot$latitude_of_centre
  plot_match <- plot$plotcode
  
  # Subset array to grid square containing plot
  lon_match_index <- which.min(abs(lon - lon_match))
  lat_match_index <- which.min(abs(lat - lat_match))
  
  temp_df <- data.frame(
    time = time,
    plot_match = plot_match,
    lon_match = lon_match,
    lat_match = lat_match,
    wind_u = ncvar_get(nc_file, "u10")[lon_match_index, lat_match_index,],
    wind_v = ncvar_get(nc_file, "v10")[lon_match_index, lat_match_index,],
    temp =  ncvar_get(nc_file, "t2m")[lon_match_index, lat_match_index,],
    temp_dew =  ncvar_get(nc_file, "d2m")[lon_match_index, lat_match_index,],
    lai_high = ncvar_get(nc_file, "lai_hv")[lon_match_index, lat_match_index,],
    lai_low = ncvar_get(nc_file, "lai_lv")[lon_match_index, lat_match_index,],
    max_temp = ncvar_get(nc_file, "mx2t")[lon_match_index, lat_match_index,],
    min_temp = ncvar_get(nc_file, "mn2t")[lon_match_index, lat_match_index,],
    surf_net_sol = ncvar_get(nc_file, "ssr")[lon_match_index, lat_match_index,],
    surf_net_sol_clear = ncvar_get(nc_file, "ssrc")[lon_match_index, lat_match_index,],
    top_net_sol = ncvar_get(nc_file, "tsr")[lon_match_index, lat_match_index,],
    top_net_sol_clear = ncvar_get(nc_file, "tsrc")[lon_match_index, lat_match_index,],
    cloud = ncvar_get(nc_file, "tcc")[lon_match_index, lat_match_index,],
    precip = ncvar_get(nc_file, "tp")[lon_match_index, lat_match_index,],
    units = nc_file$var$u10$dim[[3]]$units)
    
  temp_df$test <- gsub("hours since ", "", temp_df$units)
  temp_df$test <-
    as.POSIXct(temp_df$test, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  temp_df$time <- temp_df$test + (temp_df$time * 60 * 60)
  temp_df$date <- as.Date(temp_df$time)
  temp_df <- dplyr::select(temp_df, -test, -units)
    
  saveRDS(temp_df, file = paste0("/exports/eddie/scratch/s1108036/extracted/", plot_match, "_ecmwf_extract_2016.rds"))
}

# Import files ----
# Get netcdf file names
nc_list <- list.files(paste0("/exports/eddie/scratch/s1108036"),
  pattern = "nc$",
  full.names = TRUE)

# Plot data
load("seosaw_plot_summary5Apr2019.Rdata")
plot_loc <- ssaw8$plotInfoFull %>%
	dplyr::select(plotcode, longitude_of_centre, latitude_of_centre)

plot_loc_list <- split(plot_loc, seq(nrow(plot_loc)))
  
nc_file <- nc_open(nc_list[[1]])

# Loop over each nc file and each plot to extract raw data
mclapply(plot_loc_list, nc_extract, nc_file = nc_file, mc.cores = num_cores)

