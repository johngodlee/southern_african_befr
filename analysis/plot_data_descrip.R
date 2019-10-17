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
library(rgdal)
library(ggnewscale)

# Import data ----

# Plot data
plot_data_final <- read.csv("data/plot_data_fil_agg_norm_std_outlier.csv")

plot_data_final$fire_index <- factor(plot_data_final$fire_index, levels = c("Frequent", "Occassional", "Rare", "Very rare", "No fire"))

# Regional tmeperature and precipitation data for the Miombo woodlands
t_p <- read.csv("data/region_temp_precip.csv")

# Whites veg map
white_veg <- readOGR(dsn="/Users/johngodlee/google_drive/postgrad/phd/confirmation_report/resources/whitesveg", 
  layer="Whites vegetation")

white_veg_fort <- fortify(white_veg, region = "DESCRIPTIO")
names(white_veg_fort)
length(unique(white_veg_fort$id))

white_veg_miombo <- white_veg_fort %>%
  filter(id %in% c("Moist-infertile savanna"),
    lat < -2)

# Original data
load("data/seosaw_plot_summary5Apr2019.Rdata")

source("clust_pal.R")

# Map of plot locations, compared to full SEOSAW dataset ----

# Create vector of southern Africa ISO codes - find a way to mine the data for this
s_af <- iso.expand(c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", "MWI", "AGO", "TZA", "KEN", "COG"))
  
# Create map of country outlines
map_africa <- borders(database = "world", regions = s_af, fill = "grey90", colour = "black")
map_africa_fill <- borders(database = "world", regions = s_af, fill = "grey90")
map_africa_colour <- borders(database = "world", regions = s_af, colour = "black")

plot_map <- ggplot() + 
  map_africa_fill + 
  geom_polygon(aes(x = long, y = lat, group = group), 
    fill = "#06A2BD", colour = "#06A2BD",
    data = white_veg_miombo, alpha = 1) +
  # geom_point(data = ssaw8$plotInfoFull, aes(x = longitude_of_centre, y = latitude_of_centre), alpha = 0.6) + 
    geom_point(data = plot_data_final, 
      aes(x = longitude_of_centre, y = latitude_of_centre, fill = as.character(clust5)), 
    colour = "black", shape = 21, size = 2, alpha = 1) +
  map_africa_colour +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.2))

pdf(file = "img/plot_loc.pdf", width = 5, height = 8)
plot_map
dev.off()

# Map of plot locations in Zambia
map_zambia <- borders(database = "world", regions = iso.expand("ZMB"), fill = "grey90", colour = "black")

plot_zambia <- ggplot() + 
  map_zambia + 
  geom_point(data = filter(ssaw8$plotInfoFull, country == "Zambia"), 
    aes(x = longitude_of_centre, y = latitude_of_centre), alpha = 0.6) + 
  geom_point(data = filter(plot_data_final, country == "Zambia"),
    aes(x = longitude_of_centre, y = latitude_of_centre), 
    colour = "black", fill = "#048FD4", shape = 21, size = 2, alpha = 1) + 
  theme_classic() +
  coord_map() + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic()

pdf(file = "img/plot_loc_zambia.pdf", width = 8, height = 5)
plot_zambia
dev.off()

# Spatial distribution of key variables

var_map <- function(x, var, lab = expression("log(AGB) (t ha"^-1*")")){
  map_var <- ggplot() + 
    map_africa + 
    geom_point(data = x, aes(x = longitude_of_centre, y = latitude_of_centre, fill = var), 
      size = 2, shape = 21, colour = "black", position = "jitter") +
    scale_fill_viridis_c(name = lab) +
    coord_map() + 
    ylim(-35.5, 10) + 
    labs(x = "Longitude", y = "Latitude") + 
    theme_classic()
  
  return(map_var)
}

# Biomass
pdf(file = "img/biomass_map.pdf", width = 5, height = 8)
var_map(plot_data_final, 
  var = plot_data_final$bchave_log, 
  lab = expression("log(AGB) (t ha"^-1*")"))
dev.off()

# Aridity
pdf(file = "img/aridity_map.pdf", width = 5, height = 8)
var_map(plot_data_final, 
  var = plot_data_final$aridity_index, 
  lab = expression("Ardity" ~ "Index"))
dev.off()

# Precipitation
pdf(file = "img/precip_map.pdf", width = 5, height = 8)
var_map(plot_data_final, 
  var = plot_data_final$total_precip, 
  lab = expression("Mean" ~ "Annual" ~ "Precipitation" ~ (mm ~ y^-1)))
dev.off()

# Temp
pdf(file = "img/temp_map.pdf", width = 5, height = 8)
var_map(plot_data_final, 
  var = plot_data_final$mean_temp, 
  lab = expression("MAT" ~ (degree*C)))
dev.off()

# Soil Organic Carbon
pdf(file = "img/ocdens_map.pdf", width = 5, height = 8)
var_map(plot_data_final, 
  var = plot_data_final$ocdens, 
  lab = expression("Organic" ~ "C" ~ "(%)"))
dev.off()

# Shannon Diversity index
pdf(file = "img/shannon_map.pdf", width = 5, height = 8)
var_map(plot_data_final,
  var = plot_data_final$shannon_cube,
  lab = expression("Shannon" ~ "index" ~ (H)))
dev.off()

# Fire index
pdf(file = "img/fire_index_map.pdf", width = 5, height = 8)
ggplot() + 
  map_africa + 
  geom_point(data = plot_data_final,
    aes(x = longitude_of_centre, y = latitude_of_centre, fill = plot_data_final$fire_index), 
    size = 2, shape = 21, colour = "black", position = "jitter") +
  scale_fill_viridis_d(direction = -1) + 
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic()
dev.off()

# Biogeographic clusters
pdf(file = "img/clust_map.pdf", width = 5, height = 8)
ggplot() + 
  map_africa + 
  geom_point(data = plot_data_final, 
    aes(x = longitude_of_centre, y = latitude_of_centre, fill = as.character(plot_data_final$clust5)), 
    size = 2, shape = 21, colour = "black", position = "jitter") +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic()
dev.off()

# Relationship between temp and precip
pdf(file = "img/temp_precip.pdf", width = 6, height = 6)
ggplot() + 
  stat_binhex(data = t_p, 
    mapping = aes(x = t_vals, y = p_vals, colour = ..count.., fill = ..count..),
    bins = 500) + 
  scale_fill_continuous(name = "Density", type = "viridis", trans = "log",
    breaks = c(1, 5, 10, 50, 100, 200, 400)) + 
  scale_colour_continuous(name = "Density", type = "viridis", trans = "log", 
    breaks = c(1, 5, 10, 50, 100, 200, 400)) + 
  new_scale_fill() +
  geom_point(data = plot_data_final,
    mapping = aes(x = mean_temp, y = total_precip, fill = as.character(clust5)), 
    colour = "black", shape = 21) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  labs(x= expression("MAT" ~ (degree*C)), 
    y = expression("MAP" ~ (mm ~ y^-1))) + 
  theme(aspect.ratio = 1)
dev.off()

# How much of the climate space do the plots cover ----
# Standardize pixel values and convert to spatialpoints object
t_p_std <- data.frame(
  t_vals_std = as.vector(scale(t_p$t_vals)),
  p_vals_std = as.vector(scale(t_p$p_vals)))

t_p_std_nona <- na.omit(t_p_std)

t_p_sp <- SpatialPointsDataFrame(
  coords = t_p_std_nona, data = t_p_std_nona)

# Create a convex hull spatialpolygons object
plot_hull <- chull(plot_data_final$mean_temp_std, plot_data_final$total_precip_std)
plot_hull <- c(plot_hull, plot_hull[1])
plot_hull_data <- plot_data_final[plot_hull,]
plot_hull_coords <- plot_hull_data %>% dplyr::select(mean_temp_std, total_precip_std)

plot_hull_poly <- Polygon(plot_hull_coords, hole=F)
plot_hull_polys <- Polygons(list(plot_hull_poly), 1)
plot_hull_polys_sp = SpatialPolygons(list(plot_hull_polys))

# Count number of pixels covered by polygon
t_p_hull <- over(t_p_sp, plot_hull_polys_sp)

100 - length(t_p_hull[is.na(t_p_hull)]) / length(t_p_hull[!is.na(t_p_hull)]) * 100

# Plot the convex hull and the points together
plot_hull_fort <- fortify(plot_hull_polys_sp)

pdf(file = "img/temp_precip_hull.pdf", width = 6, height = 6)
ggplot() + 
  stat_binhex(data = t_p_std, 
    mapping = aes(x = t_vals_std, y = p_vals_std, colour = ..count.., fill = ..count..),
    bins = 500) + 
  scale_fill_continuous(name = "Density", type = "viridis", trans = "log",
    breaks = c(1, 5, 10, 50, 100, 200, 400)) + 
  scale_colour_continuous(name = "Density", type = "viridis", trans = "log", 
    breaks = c(1, 5, 10, 50, 100, 200, 400)) + 
  new_scale_fill() +
  geom_point(data = plot_data_final,
    mapping = aes(x = mean_temp_std, y = total_precip_std, fill = as.character(clust5)), 
    colour = "black", shape = 21) + 
  geom_polygon(data = plot_hull_fort,
    aes(x = long, y = lat), fill = NA, colour = "#A3152A") + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  labs(x= expression("MAT" ~ (degree*C)), 
    y = expression("MAP" ~ (mm ~ y^-1))) + 
  theme(aspect.ratio = 1) 
dev.off()

#How many countries was the original data in?
unique(ssaw8$plotInfoFull$country)
length(unique(ssaw8$plotInfoFull$country))

# How many countries is our data in?
unique(plot_data_final$country)
length(unique(plot_data_final$country))

# How many plots did the original data have?
length(unique(ssaw8$plotInfoFull$plotcode))

# How many plots does the clean dataset have?
length(unique(plot_data_final$plot_group))

# What is the range of precipitation values?
ggplot(plot_data_final, aes(x = total_precip)) + 
  geom_histogram(fill = "gray", colour = "black")

tail(sort(plot_data_final$total_precip), n = 20)
head(sort(plot_data_final$total_precip), n = 20)

# Make cluster non-ordinal
plot_data_final$clust5 <- as.character(plot_data_final$clust5)

# Remove plots not in a cluster
plot_data_final <- plot_data_final %>%
  filter(!is.na(clust5))

# Plot all variables against bchave_log in a facet wrap ----
# Climatic variables
plot_data_gather_clim <- plot_data_final %>%
  select(clust5, bchave_log, total_precip, precip_seasonality, mean_temp, temp_seasonality, 
    fire_return_mean_log,
    ocdens, sand_per, cation_ex_cap) %>%
  gather(key = "variable", value = "value", 3:10) %>%
  mutate(facet_label = factor(variable, 
    levels = c("total_precip", "precip_seasonality", "mean_temp", "temp_seasonality", 
      "fire_return_mean_log", "ocdens", "sand_per", "cation_ex_cap"),
    labels = c(
      expression("Mean" ~ "annual" ~ "precip." ~ (mm ~ yr^-1)),
      expression("Precip." ~ "seasonality"),
      expression("Mean" ~ "annual" ~ "temp." ~ (degree * C)), 
      expression("Temp." ~ "seasonality"),
      expression("log(Mean" ~ "fire" ~ "return" ~ "interval)" ~ (yr)),
      expression("Organic" ~ "C" ~ "%"),
      expression("Sand" ~ "%"),
      expression("Cation" ~ "exchange" ~ "cap."))))

pdf(file = "img/biomass_clim_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_gather_clim, aes(x = value, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) +
  geom_smooth(method = "lm", colour = "black") +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  facet_wrap(~facet_label, scales = "free_x", labeller = label_parsed)
dev.off()

# Diversity variables
plot_data_gather_div <- plot_data_final %>%
  select(clust5, bchave_log, mean_height, cov_height, mean_dbh_log,
    cov_dbh, stems_ha_log, shannon_cube, sp_rich, sp_rich_raref) %>%
  gather(key = "variable", value = "value", 3:10) %>%
  mutate(facet_label = factor(variable, 
    levels = c("mean_height", "cov_height", "mean_dbh_log", "cov_dbh", 
      "stems_ha_log", "shannon_cube", "shannon_equit", "sp_rich", "sp_rich_raref"),
    labels = c(
      expression("Mean" ~ "height" ~ (m)),
      expression("Coef." ~ "var." ~ "height"),
      expression("log(Mean" ~ "DBH)" ~ (cm)),
      expression("Coef." ~ "var." ~ "DBH"),
      expression("log(Stem" ~ "density)" ~ ">5" ~ cm ~ (n ~ ha^-1)),
      expression("Shannon" ~ "index" ~ (H)),
      expression("Shannon" ~ "equitability" ~ (E[H])),
      expression("Species" ~ "richness"),
      expression("Rarefied" ~ "species" ~ "richness")
    )))

pdf(file = "img/biomass_div_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_gather_div, aes(x = value, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  facet_wrap(~variable, scales = "free_x", labeller = label_parsed)
dev.off()

pdf(file = "img/biomass_sp_rich_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_final, aes(x = sp_rich, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) +
  labs(x = "Species richness", y = expression("AGB" ~ (t ~ ha^-1))) + 
  theme_classic()
dev.off()

# Investigating species richness vs. biomass
pdf(file = "img/biomass_sp_rich_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_final, aes(x = sp_rich_raref, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) +
  labs(x = "Rarefied Species richness", y = expression("AGB" ~ (t ~ ha^-1))) + 
  theme_classic()
dev.off()
