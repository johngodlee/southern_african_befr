# Description of cleaned data
# John Godlee (johngodlee@gmail.com)
# 2019_09_19

# Preamble ----

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)

# Import data ----

# Plot data
plot_data_final <- read.csv("data/plot_data_fil_agg_norm_std_outlier.csv")

# Original data
load("data/seosaw_plot_summary5Apr2019.Rdata")

# Map of plot locations, compared to full SEOSAW dataset ----

# Create vector of southern Africa ISO codes - find a way to mine the data for this
s_af <- iso.expand(c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", "MWI", "AGO", "TZA", "KEN", "COG"))
  
# Create map of country outlines
map_africa <- borders(database = "world", regions = s_af, fill = "grey90", colour = "black")
  
plot_map <- ggplot() + 
  map_africa + 
    geom_point(data = ssaw8$plotInfoFull, aes(x = longitude_of_centre, y = latitude_of_centre), alpha = 0.6) + 
    geom_point(data = plot_data_final, aes(x = longitude_of_centre, y = latitude_of_centre), 
    colour = "black", fill = "#048FD4", shape = 21, size = 2, alpha = 1) +
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic()

pdf(file = "img/plot_loc.pdf", width = 5, height = 8)
plot_map
dev.off()

# Map of plot locations in Zambia
map_zambia <- borders(database = "world", regions = iso.expand("ZMB"), fill = "grey90", colour = "black")

(plot_zambia <- ggplot() + 
  map_zambia + 
  geom_point(data = filter(ssaw8$plotInfoFull, country == "Zambia"), 
    aes(x = longitude_of_centre, y = latitude_of_centre), alpha = 0.6) + 
  geom_point(data = filter(plot_data_final, country == "Zambia"),
    aes(x = longitude_of_centre, y = latitude_of_centre), 
    colour = "black", fill = "#048FD4", shape = 21, size = 2, alpha = 1) + 
  theme_classic() +
  coord_map() + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic())

pdf(file = "img/plot_loc_zambia.pdf", width = 8, height = 5)
plot_zambia
dev.off()

# Spatial distribution of key variables

# Biomass
biomass_map <- ggplot() + 
  map_africa + 
  geom_point(data = plot_data_final, aes(x = longitude_of_centre, y = latitude_of_centre, fill = bchave_log), 
    size = 2, shape = 21, colour = "black", position = "jitter") +
  scale_fill_viridis_c(name = expression("log(AGB) (t ha"^-1*")")) +
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic()

pdf(file = "img/biomass_map.pdf", width = 5, height = 8)
biomass_map
dev.off()
