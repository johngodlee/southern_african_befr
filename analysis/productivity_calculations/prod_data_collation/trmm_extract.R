# Extract data from TRMM precipitation netCDF files for SEOSAW plots
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
library(stringr)

# Import netcdf files ----
# Get file names
nc_list <- list.files(paste0(getwd(),"/raw_data/trmm_nc"), 
                      pattern="nc4$", 
                      full.names=FALSE)

# Import files directly into list
nc_file_list <- list()

from <- match("3B42_Daily.20100101.7.nc4.nc4", nc_list)
to <- match("3B42_Daily.20171231.7.nc4.nc4", nc_list)


# Import SEOSAW plot locations ----
load("raw_data/seosaw_plot_summary24Sept_v1.Rdata")

plot_loc <- ssaw7$plotInfoFull %>%
  dplyr::select(plotcode, longitude_of_centre, latitude_of_centre)

# For testing, isolate 3 sets of plot coords ----
plot_match <- c("ABG-001", "ZLT-009", "MAR-030")
plot_test <- plot_loc[grep(paste(plot_match, collapse = "|"), plot_loc$plotcode),]
plot_test$plotcode <- as.character(plot_test$plotcode)
plot_loc_list <- split(plot_test, seq(nrow(plot_test)))


# nc_file <- nc_open(paste0("raw_data/trmm_nc/", nc_list[1]))

extracted_trmm_all <- list()
for(i in 1:length(plot_loc_list)){
  all_df <- NULL  # temp df for each plot
  lon_match <- plot_loc_list[[i]]$longitude_of_centre
  lat_match <- plot_loc_list[[i]]$latitude_of_centre
  plot_match <- plot_loc_list[[i]]$plotcode
  
  for(j in nc_list[from:to]){ 
    nc_file <- nc_open(paste0("raw_data/trmm_nc/", j))
    
    # Get grid 
    lon <- ncvar_get(nc_file, "lon", verbose = FALSE)
    lat <- ncvar_get(nc_file, "lat", verbose = FALSE)
    filename <- nc_file$filename
    
    # Create matrix
    precip_mat <- ncvar_get(nc_file, "precipitation")
    
    # Subset matrix to grid square containing plot
    lon_match_index <- which.min(abs(lon-lon_match))
    lat_match_index <- which.min(abs(lat-lat_match))
    
    temp_df <- data.frame(filename = filename,
                          plot_match = plot_match,
                          lon_match = lon_match,
                          lat_match = lat_match,
                          precip = precip_mat[lon_match_index, lat_match_index],
                          units = nc_file$var$precipitation$units)
    
    all_df <- rbind(all_df, temp_df)
    
    nc_close(nc_file)
  }
  extracted_trmm_all[[i]] <- all_df
  
  extracted_trmm_all[[i]]$date <- as.Date(
    unlist(
      lapply(
        str_extract_all(
          extracted_trmm_all[[i]]$filename, "\\(?[0-9]+\\)?"),
        '[[', 3)),
    format = "%Y%m%d")
}

save(extracted_trmm_all, file = "extracted_data/extracted_trmm_all.Rdata")

