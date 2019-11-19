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
library(gridExtra)

# Import data ----

# Plot data
plot_data_final <- read.csv("data/plot_data_fil_agg_norm_std_outlier.csv")

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

# Biomass quantiles
bchave_quantile_df <- read.csv("data/bchave_quantile.csv")

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
      aes(x = longitude_of_centre, y = latitude_of_centre, fill = paste0("C", as.character(clust5))), 
    colour = "black", shape = 21, size = 2, alpha = 1) +
  map_africa_colour +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic() +
  theme(legend.position = "none")
  # theme(legend.position = c(0.9, 0.2))

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
  var = plot_data_final$bchave, 
  lab = expression("log(AGB) (t ha"^-1*")"))
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
  var = plot_data_final$shannon_log,
  lab = expression("Shannon" ~ "index" ~ (H)))
dev.off()

# Fire return
pdf(file = "img/fire_index_map.pdf", width = 5, height = 8)
var_map(plot_data_final,
  var = plot_data_final$fire_return_mean_log,
  lab = expression("log(Fire" ~ "Return" ~ "interval)"))
dev.off()

plot_data_final$clust5_plot <- as.character(paste0("C",plot_data_final$clust5))

# Biogeographic clusters
pdf(file = "img/clust_map.pdf", width = 5, height = 8)
ggplot() + 
  map_africa + 
  geom_point(data = plot_data_final, 
    aes(x = longitude_of_centre, y = latitude_of_centre, fill = clust5_plot), 
    size = 2, shape = 21, colour = "black", position = "jitter") +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  facet_wrap(~clust5_plot) + 
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic() + 
  theme(legend.position = "none")
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
plot_hull <- chull(plot_data_final$mean_temp_rev_std, plot_data_final$total_precip_std)
plot_hull <- c(plot_hull, plot_hull[1])
plot_hull_data <- plot_data_final[plot_hull,]
plot_hull_coords <- plot_hull_data %>% dplyr::select(mean_temp_rev_std, total_precip_std)

plot_hull_poly <- Polygon(plot_hull_coords, hole=F)
plot_hull_polys <- Polygons(list(plot_hull_poly), 1)
plot_hull_polys_sp = SpatialPolygons(list(plot_hull_polys))

# Count number of pixels covered by polygon
t_p_hull <- over(t_p_sp, plot_hull_polys_sp)

pixel_cover <- round(100 - length(t_p_hull[is.na(t_p_hull)]) / length(t_p_hull[!is.na(t_p_hull)]) * 100, 
  digits = 1)

fileConn <- file(paste0("output/include/hull_cover.tex"))
writeLines(
    paste0("\\newcommand{\\hullcover}{", pixel_cover, "}"),
  fileConn)
close(fileConn)
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
    mapping = aes(x = mean_temp_rev_std, y = total_precip_std, fill = as.character(clust5_plot)), 
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
n_plots_num <- length(unique(plot_data_final$plot_group))

# Plot number pre-PCOA
n_plots_pcoa <- n_plots_num + 66

fileConn <- file(paste0("output/include/n_plots.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\nplots}{", n_plots_num, "}"),
    paste0("\\newcommand{\\nplotspcoa}{", n_plots_pcoa, "}")),
  fileConn)
close(fileConn)

# What is the range of precipitation values?
ggplot(plot_data_final, aes(x = total_precip)) + 
  geom_histogram(fill = "gray", colour = "black")

tail(sort(plot_data_final$total_precip), n = 20)
head(sort(plot_data_final$total_precip), n = 20)

# Where are the wettest plots found?
plot_data_precip_max_min <- plot_data_final %>%
  filter(total_precip > quantile(total_precip, 0.9) | 
      total_precip < quantile(total_precip, 0.1)) %>%
  mutate(hi_lo = case_when(
    total_precip > median(plot_data_final$total_precip) ~ "hi",
    total_precip < median(plot_data_final$total_precip) ~ "lo"
  ))

pdf(file = "img/precip_extremes_map.pdf", width = 5, height = 8)
ggplot() + 
  map_africa_fill + 
  geom_polygon(aes(x = long, y = lat, group = group), 
    fill = "#16CC7D", colour = "#16CC7D",
    data = white_veg_miombo, alpha = 1) +
  geom_point(data = plot_data_precip_max_min, 
    aes(x = longitude_of_centre, y = latitude_of_centre, fill = hi_lo), 
    colour = "black", shape = 21, size = 2, alpha = 1) +
  map_africa_colour +
  scale_fill_manual(name = "Precip.", values =  c("#56CDD6", "#DECA59"))+ 
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.2))
dev.off()

# How many stems are used?
sum(plot_data_final$n_stems)


# Make cluster non-ordinal
plot_data_final$clust5 <- as.character(plot_data_final$clust5)

# Remove plots not in a cluster
plot_data_final <- plot_data_final %>%
  filter(!is.na(clust5))

# Plot all variables against bchave_log in a facet wrap ----
# Climatic variables
plot_data_gather_clim <- plot_data_final %>%
  select(clust5, bchave_log, total_precip, precip_seasonality_log, mean_temp, temp_seasonality_log, 
    fire_return_mean_log,
    ocdens, sand_per, cation_ex_cap) %>%
  gather(key = "variable", value = "value", 3:10)
pdf(file = "img/biomass_clim_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_gather_clim, aes(x = value, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) +
  geom_smooth(method = "lm", colour = "black") +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  facet_wrap(~variable, scales = "free_x", labeller = label_parsed)
dev.off()

# Diversity variables
plot_data_gather_div <- plot_data_final %>%
  select(clust5, bchave_log, mean_height, cov_height, mean_dbh_log,
    cov_dbh, stems_ha_log, shannon_log, sp_rich, sp_rich_raref_log) %>%
  gather(key = "variable", value = "value", 3:10)

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

# How do extrapolated species richnesses compare to actual?
pdf(file = "img/sp_rich_vs_raref_clust.pdf", width = 12, height = 7)
ggplot(plot_data_final, aes(x = sp_rich, y = sp_rich_raref)) + 
  geom_point(aes(colour = clust5), 
    alpha = 0.6) + 
  geom_abline(aes(intercept = 0, slope = 1), 
    linetype = 2) + 
  facet_wrap(~clust5, nrow = 1) + 
  theme_classic() +
  coord_equal()
dev.off()

plot_data_final[which.max(plot_data_final$sp_rich),]

plot_data_final[which.min(plot_data_final$sp_rich_raref),]

# Investigating DBH evenness measures and relationship with other variables
plot_data_final_gather <- plot_data_final %>%
  dplyr::select(diam_even_std, ends_with("_std")) %>%
  gather("var", "value", -diam_even_std)

pdf(file = paste0("img/diam_even_bivar", ".pdf"), width = 18, height = 18)
ggplot(plot_data_final_gather, aes(x = diam_even_std, y = value)) + 
  geom_point(alpha = 0.6) + 
  stat_smooth(colour = "red") +
  stat_smooth(method = "lm") + 
  facet_wrap(~var, scales = "free_y") + 
  theme_classic()
dev.off()

# Species richness as determinant of biomass in large trees
##' measured as mean 95th percentile of height

sp_bchave <- ggplot(plot_data_final, aes(x = sp_rich, y = bchave_log)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("S ~ log(AGB)") + 
  theme_classic()

sp_bchave_big <- ggplot(plot_data_final, aes(x = sp_rich, y = log(bchave_mean_95))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("S ~ log(AGB big trees)") + 
  theme_classic()

sp_raref_bchave <- ggplot(plot_data_final, aes(x = sp_rich_raref, y = bchave_log)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("S raref ~ log(AGB)") + 
  theme_classic()

sp_raref_bchave_big <- ggplot(plot_data_final, aes(x = sp_rich_raref, y = log(bchave_mean_95))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("S raref ~ log(AGB big trees)") + 
  theme_classic()

log_sp_raref_bchave <- ggplot(plot_data_final, aes(x = log(sp_rich_raref), y = bchave_log)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("log(S raref) ~ log(AGB)") + 
  theme_classic()

log_sp_raref_bchave_big <- ggplot(plot_data_final, aes(x = log(sp_rich_raref), y = log(bchave_mean_95))) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("log(S raref) ~ log(AGB big trees)") + 
  theme_classic()


pdf(file = "img/big_trees_bchave_sp_rich.pdf", width = 18, height = 18)
grid.arrange(sp_bchave, sp_bchave_big, 
  sp_raref_bchave, sp_raref_bchave_big,
  log_sp_raref_bchave, log_sp_raref_bchave_big, nrow = 3)
dev.off()

# Biomass quantiles and their relationship with Species richness

bchave_quantile_df$sp_rich_raref_std <- scale(bchave_quantile_df$sp_rich_raref)

bchave_quantile_df_gather <- bchave_quantile_df %>% 
  gather("quantile", "value", -plot_group, -sp_rich_raref, -sp_rich, -sp_rich_raref_std)

quantile_labs <- paste0(quantile_lo, "-", (seq(from = 0, to = 0.95, by = 0.05) + 0.05) * 100)

bchave_quantile_df_gather$quantile <- factor(bchave_quantile_df_gather$quantile,
  levels = c("bchave_mean_95_0.0.05", "bchave_mean_95_0.05.0.1", "bchave_mean_95_0.1.0.15",
    "bchave_mean_95_0.15.0.2", "bchave_mean_95_0.2.0.25", "bchave_mean_95_0.25.0.3",
    "bchave_mean_95_0.3.0.35", "bchave_mean_95_0.35.0.4", "bchave_mean_95_0.4.0.45",
     "bchave_mean_95_0.45.0.5", "bchave_mean_95_0.5.0.55", "bchave_mean_95_0.55.0.6",
     "bchave_mean_95_0.6.0.65", "bchave_mean_95_0.65.0.7", "bchave_mean_95_0.7.0.75",
     "bchave_mean_95_0.75.0.8", "bchave_mean_95_0.8.0.85", "bchave_mean_95_0.85.0.9",
     "bchave_mean_95_0.9.0.95", "bchave_mean_95_0.95.1"),
  labels = quantile_labs)


pdf(file = "img/bchave_quantile_slope_sp_rich_raw.pdf", width = 12, height = 10)
ggplot(bchave_quantile_df_gather, aes(x = sp_rich_raref, y = log(value))) + 
  geom_point(aes(colour = quantile), alpha = 0.6) + 
  geom_smooth(method = "lm", colour = "black") + 
  facet_wrap(~quantile, scales = "fixed", labeller = labeller()) +
  labs(x = "Extrapolated species richness\nof plot", y = "log(AGB of percentile)") + 
  theme_classic() + 
  theme(legend.position = "none")
dev.off()

bchave_quantile_lm_list <- list()
for(i in names(bchave_quantile_df)[startsWith(names(bchave_quantile_df), "bchave_mean_95_0")]){
  bchave_quantile_lm_list[[i]] <- lm(get(i) ~ sp_rich_raref, 
    data = bchave_quantile_df)
}

bchave_quantile_lm_coef <- sapply(bchave_quantile_lm_list, function(x){x$coefficients[2]})
bchave_quantile_lm_se <- sapply(bchave_quantile_lm_list, function(x){summary(x)$coefficients[2,2]})

quantile_lo <- seq(from = 0, to = 0.95, by = 0.05) * 100

bchave_quantile_lm_coef_df <- data.frame(quantile_lo, bchave_quantile_lm_coef,
  bchave_quantile_lm_se)


pdf(file = "img/bchave_quantile_slope_sp_rich.pdf", width = 8, height = 5)
ggplot() + 
  geom_point(data = bchave_quantile_lm_coef_df, 
    aes(x = quantile_lo, y = bchave_quantile_lm_coef)) + 
  geom_errorbar(data = bchave_quantile_lm_coef_df, 
    aes(x = quantile_lo, 
      ymax = bchave_quantile_lm_coef + bchave_quantile_lm_se, 
      ymin =  bchave_quantile_lm_coef - bchave_quantile_lm_se)) +
  theme_classic() + 
  labs(x = "Percentile of tree size\nmeasured by AGB", y = "Slope of linear model\n(AGB ~ Species rich.)") + 
  scale_x_continuous(breaks = quantile_lo, labels = quantile_labs) + 
  theme(axis.text.x=element_text( 
    angle=45, 
    vjust=1, 
    hjust=1)) 
dev.off()




