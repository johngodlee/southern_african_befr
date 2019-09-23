# Extract data from ECMWF netCDF files for SEOSAW plots
# John Godlee (johngodlee@gmail.com)
# 2018_10_30

# Preamble ----

# Remove old crap
rm(list=ls())

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)
library(ncdf4)

# Import netcdf files ----
# Get file names
nc_list <- list.files(paste0(getwd(),"/raw_data/ecmwf/ecmwf_nc"), 
                       pattern="nc$", 
                       full.names=FALSE)

# Import files directly into list
nc_file_list <- list()

for(i in nc_list){ 
  nc_file_list[[i]] <- nc_open(paste0("raw_data/ecmwf/ecmwf_nc/", i))
} 

# Import SEOSAW plot locations ----
load("raw_data/seosaw_plot_summary24Sept_v1.Rdata")

plot_loc <- ssaw7$plotInfoFull %>%
  dplyr::select(plotcode, longitude_of_centre, latitude_of_centre)

# For testing, isolate 3 sets of plot coords ----
plot_match <- c("ABG-001", "ZLT-009", "MAR-030")
plot_test <- plot_loc[grep(paste(plot_match, collapse = "|"), plot_loc$plotcode),]
plot_test$plotcode <- as.character(plot_test$plotcode)
plot_loc_list <- split(plot_test, seq(nrow(plot_test)))



# Automate extraction of sol in a for loop ---- 
##' Read sol for each timestep across all months for SEOSAW plots
##' List of dataframes
##' Each dataframe contains data for one plot
##' Columns for plotcode, lon, lat, timestep, sol

extracted_ecmwf_all <- list()
for(i in 1:length(plot_loc_list)){
  all_df <- NULL  # temp df for each plot
  lon_match <- plot_loc_list[[i]]$longitude_of_centre
  lat_match <- plot_loc_list[[i]]$latitude_of_centre
  plot_match <- plot_loc_list[[i]]$plotcode
  
  for(j in 1:length(nc_file_list)){
    # Get grid 
    lon <- ncvar_get(nc_file_list[[j]], "lon", verbose = FALSE)
    lat <- ncvar_get(nc_file_list[[j]], "lat", verbose = FALSE)
    time <- ncvar_get(nc_file_list[[j]], "time", verbose = FALSE)
    
    # Create array
    sol_array <- ncvar_get(nc_file_list[[j]], "var178") 
    max_temp_array <- ncvar_get(nc_file_list[[j]], "var201") 
    min_temp_array <- ncvar_get(nc_file_list[[j]], "var202") 
    net_sol_array <- ncvar_get(nc_file_list[[j]], "var208") 
    net_sol_clear_array <- ncvar_get(nc_file_list[[j]], "var210") 
    precip_array <- ncvar_get(nc_file_list[[j]], "var228") 
    sol_down_array <- ncvar_get(nc_file_list[[j]], "VARAGSGAGCCCCXXVAR")
    
    # Subset array to grid square containing plot
    lon_match_index <- which.min(abs(lon-lon_match))
    lat_match_index <- which.min(abs(lat-lat_match))
    
    temp_df <- data.frame(time = time,
                               plot_match = plot_match,
                               lon_match = lon_match,
                               lat_match = lat_match,
                               sol = sol_array[lon_match_index, lat_match_index,],
                               max_temp = max_temp_array[lon_match_index, lat_match_index,],
                               min_temp = min_temp_array[lon_match_index, lat_match_index,],
                               net_sol = net_sol_array[lon_match_index, lat_match_index,],
                               net_clear_sol = net_sol_clear_array[lon_match_index, lat_match_index,],
                               precip = precip_array[lon_match_index, lat_match_index,],
                               sol_down = sol_down_array[lon_match_index, lat_match_index,],
                               units = nc_file_list[[j]]$var$var178$dim[[3]]$units)
    
    temp_df$test <- gsub("hours since ", "", temp_df$units)
    temp_df$test <- as.POSIXct(temp_df$test, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    temp_df$time <- temp_df$test + (temp_df$time * 60 * 60)
    temp_df$date <- as.Date(temp_df$time)
    temp_df <- dplyr::select(temp_df, -test, -units)

    all_df <- rbind(all_df, temp_df)  # bind plot to all 
   
  }
  extracted_ecmwf_all[[i]] <- all_df
}

# Create a mean for each day

extracted_ecmwf_all_daily <- list()
for(i in 1:length(extracted_ecmwf_all)){
  temp <- extracted_ecmwf_all[[i]] %>%
    group_by(date, plot_match, lon_match, lat_match) %>%
    summarise(mean_sol = sum(sol),
              max_temp = max(max_temp),
              min_temp = min(min_temp), 
              total_net_sol = sum(net_sol),
              total_net_clear_sol = sum(net_clear_sol),
              total_precip = sum(precip),
              toal_sol_down = sum(sol_down))
  extracted_ecmwf_all_daily[[i]] <- temp
  
}

save(extracted_ecmwf_all_daily, file = "extracted_data/extracted_ecmwf_all_daily.Rdata")

