# Extract soil data from SEOSAW summary data - ISRIC soil grids
# John Godlee (johngodlee@gmail.com)
# 2018_10_25

# Preamble ----

# Remove old crap
rm(list=ls())

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data ----

load("../seosaw_plot_summary5Apr_v1.Rdata")

# Filter data to soil properties ----
soil <- data.frame(plotcode = ssaw7$soil$plotcode, 
                   longitude_of_centre = ssaw7$soil$longitude_of_centre.x,
                   latitude_of_centre = ssaw7$soil$latitude_of_centre.x,
                   CLYPPT_M_sl1_1km_ll = ssaw7$soil$CLYPPT_M_sl1_1km_ll,
                   SLTPPT_M_sl1_1km_ll = ssaw7$soil$SLTPPT_M_sl1_1km_ll,
                   SNDPPT_M_sl1_1km_ll = ssaw7$soil$SNDPPT_M_sl1_1km_ll,
                   ORCDRC_M_sl1_1km_ll = ssaw7$soil$ORCDRC_M_sl1_1km_ll,
                   CECSOL_M_sl1_1km_ll = ssaw7$soil$CECSOL_M_sl1_1km_ll)

# Write data ----
write.csv(soil, "../extracted_data/soil_data_plots.csv", row.names = FALSE)