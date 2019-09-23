# Extract LAI pixel values from copernicus tif files
# John Godlee (johngodlee@gmail.com)
# 2018_10_16

# Preamble ----

# Remove old crap
rm(list=ls())

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(raster)
library(ggplot2)
library(plyr)
library(dplyr)

# Import tif files ----
# Get file names
tif_list <- list.files(paste0(getwd(),"/raw_data/copernicus_tif"), 
                       pattern="tif$", 
                       full.names=FALSE)

# Import files directly into list
tif_file_list <- list()

for(i in tif_list){ 
  tif_file_list[[i]] <- raster(paste0("raw_data/copernicus_tif/", i))
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

# Extract LAI from all tif files for plot locations ----

# Create an empty data frame which will hold all the data
extracted_data_plot <- data.frame(plotcode = character(0), 
                                 long = double(0), 
                                 lat = double(0), 
                                 pixel_value = integer(0),
                                 date = as.Date(character(0)), stringsAsFactors = FALSE)

# Create an empty object to rbind results of each loop run to
extracted_data_all <- NULL

# Nested for loop to extract LAI from each tif (j) for each plot (i)
for(i in 1:length(plot_loc_list)){
  for(j in 1:length(tif_file_list)){
    
    extracted_data_plot[j,1] <- plot_loc_list[[i]]$plotcode
    extracted_data_plot[j,2] <- plot_loc_list[[i]]$longitude_of_centre
    extracted_data_plot[j,3] <- plot_loc_list[[i]]$latitude_of_centre
    extracted_data_plot[j,4] <- extract(tif_file_list[[j]],
                                        as.matrix(plot_loc_list[[i]][,2:3]),
                                        method = 'simple')
    extracted_data_plot[j,5] <- as.Date(gsub("c_gls_LAI300_QL_|0000_GLOBE_PROBAV_V1.0.1", 
                                             "", 
                                             names(tif_file_list[[j]])), format = "%Y%m%d")
    
  }
  extracted_data_all <- rbind(extracted_data_all, extracted_data_plot)
}

# extracted_data_all should be 228 rows long (76 tif files * 3 plots)
dim(extracted_data_all)

# Convert pixel digital value to physical value, as per Copernicus instructions 
##' https://land.copernicus.eu/global/products/lai
extracted_data_all$physical_value <- extracted_data_all$pixel_value * (1/30)

extracted_data_all$day_of_year <- as.numeric(format(extracted_data_all$date, format = "%j"))
extracted_data_all$year <- format(extracted_data_all$date, format = "%Y")

# Visualise splines on ggplot for three plots
# Split the dataframe into a list of dataframes
extrac_list <- split(extracted_data_all, f = extracted_data_all$plotcode)

# Calculate splines
spline_list <- list()
for(i in 1:length(extrac_list)){
  extract <- extrac_list[[i]] %>%
    filter(!is.na(date), 
           !is.na(physical_value))
  
  spline <- smooth.spline(as.numeric(extract$date), extract$physical_value, df = 10)
  
  spline_list[[i]] <- data.frame(date_num = spline$x, physical_value = spline$y, plotcode = extract$plotcode)
}

# Combine dataset
spline_df <- rbind.fill(spline_list)

# Plot ggplots
ggplot(extracted_data_all, aes(x = as.numeric(date), y = physical_value, colour = plotcode, fill = plotcode)) + 
  geom_point() +
  geom_smooth(method = "loess", span = 0.2) + 
  geom_line(data = spline_df, aes(x = date_num, y = physical_value, colour = plotcode), linetype = 3, size = 1)
 
ggplot(extracted_data_all, aes(x = day_of_year, y = physical_value, colour = plotcode, fill = plotcode)) + 
  geom_point() +
  geom_smooth(method = "loess", span = 0.8)

# Create an empty list to fill with predicted values
predict_list <- list()

for(i in 1:length(extrac_list)){
  
  extract <- extrac_list[[i]] %>%
    filter(!is.na(date),
           !is.na(physical_value))
  
  loess_model <- loess(physical_value ~ as.numeric(date), data = extract, span = 0.8)
  
  lai_predic_loess <- predict(object = loess_model, newdata = seq(from = min(as.numeric(extract$date), na.rm = TRUE), 
                                           to = max(as.numeric(extract$date), na.rm = TRUE)))
  
  spline_model <- smooth.spline(as.numeric(extract$date), extract$physical_value, df = 10)
  
  lai_predic_spline <- predict(object = spline_model, x = seq(from = min(as.numeric(extract$date), na.rm = TRUE), 
                                                                  to = max(as.numeric(extract$date), na.rm = TRUE)))
  
  predicted_lai <- data.frame(plotcode = plot_loc_list[[i]]$plotcode, lai_predic_loess, lai_predic_spline = lai_predic_spline$y,
                              date = seq(from = min(as.numeric(extract$date), na.rm = TRUE), 
                                         to = max(as.numeric(extract$date), na.rm = TRUE)))
  
  predict_list[[i]] <- predicted_lai
  
  predict_list[[i]]$date = as.Date(predict_list[[i]]$date, origin = "1970-01-01")
}



# Save as data
save(predict_list, file = "extracted_data/lai_interp_all.Rdata")
