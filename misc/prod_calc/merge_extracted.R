# Aggregate all data sources
# John Godlee (johngodlee@gmail.com)
# 2018_10_30

# Preamble ----

# Remove old crap
rm(list = ls())

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(plyr)
library(dplyr)

# Load data ----

extracted_ecmwf_df <- readRDS("extracted_data/ecmwf_extrac_df.rds")

soil <- read.csv("extracted_data/soil_data_plots.csv")

# Subset for testing to three plots
soil_test <- soil %>%
  filter(plotcode %in% c("ABG-001", "MAR-030", "ZLT-009"))

# Join datasets

all_data <- extracted_ecmwf_df %>%
  left_join(., soil_test, by = c("plot_match" = "plotcode"))

# Clean joined dataset
str(all_data)

all_data_clean <- all_data %>%
  dplyr::select(
    date,
    plot_match,
    lon = lon_match,
    lat = lat_match,
    wind_speed_mean,
    lai_high_mean,
    lai_low_mean,
    temp_mean,
    max_temp,
    min_temp,
    vpd_mean,
    surf_net_sol_sum,
    surf_net_sol_clear_sum,
    top_net_sol_sum,
    top_net_sol_clear_sum,
    cloud_mean,
    precip_sum,
    CLYPPT_M_sl1_1km_ll,
    CLYPPT_M_sl7_1km_ll,
    SLTPPT_M_sl1_1km_ll,
    SLTPPT_M_sl7_1km_ll,
    SNDPPT_M_sl1_1km_ll,
    SNDPPT_M_sl7_1km_ll,
    ORCDRC_M_sl1_1km_ll,
    ORCDRC_M_sl7_1km_ll,
    CECSOL_M_sl1_1km_ll,
    CECSOL_M_sl7_1km_ll
  )

write.csv(all_data_clean,
  "extracted_data/all_data_compiled.csv", 
  row.names = FALSE)
