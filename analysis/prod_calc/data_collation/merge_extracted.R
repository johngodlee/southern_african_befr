# Aggregate all data sources 
# John Godlee (johngodlee@gmail.com)
# 2018_10_30

# Preamble ----

# Source these files to rebuild data inputs - 
 source("lai_extract.R")
 source("ecmwf_extract.R")
 source("trmm_extract.R")
 source("soil_extract.R")

# Remove old crap
rm(list=ls())

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(plyr)
library(dplyr)

# Load data ----

load("extracted_data/extracted_ecmwf_all_daily.Rdata")

load("extracted_data/extracted_trmm_all.Rdata")

soil <- read.csv("extracted_data/soil_data_plots.csv")

# Subset for testing to three plots
soil_test <- soil %>%
  filter(plotcode %in% c("ABG-001", "MAR-030", "ZLT-009"))

load("extracted_data/lai_interp_all.Rdata")

# Make sure all are flat dataframes ----

# ECMWF
extracted_ecmwf_df <- rbind.fill(extracted_ecmwf_all_daily)

# LAI
extracted_lai_df <- rbind.fill(predict_list)

# TRMM
extracted_trmm_df <- rbind.fill(extracted_trmm_all)


# Join datasets

all_data <- extracted_ecmwf_df %>%
  left_join(., extracted_lai_df, by = c("plot_match" = "plotcode", "date" = "date")) %>%
  left_join(., soil_test, by = c("plot_match" = "plotcode")) %>%
  left_join(., extracted_trmm_df, by = c("plot_match" = "plot_match", "date" = "date"))

# Clean joined dataset
str(all_data)

all_data_clean <- all_data %>%
  dplyr::select(date, plot_match, lon = lon_match.x, lat = lat_match.x, mean_sol, 
                max_temp, min_temp, total_net_sol, total_net_clear_sol, 
                precip_ecmwf = total_precip, 
                lai_predic_loess, lay_predic_spline, CLYPPT_M_sl1_1km_ll, SLTPPT_M_sl1_1km_ll,
                SNDPPT_M_sl1_1km_ll, ORCDRC_M_sl1_1km_ll, CECSOL_M_sl1_1km_ll, 
                precip_trmm = precip)


write.csv(all_data_clean, "output_data/all_data_compiled.csv", row.names = FALSE)