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
library(dplyr)  #
library(ggplot2)  #
library(tidyr)  #
library(maps)  #
library(rgdal)  #
library(ggnewscale)  # 
library(stargazer)  #
library(labdsv)  #

source("scripts/clust_defin.R")

# Import data ----

# Plot data
plot_data <- readRDS("data/plot_data_fil_agg_norm_std_outlier.rds")

names(clust_names) <- c(1,2,3,4)
plot_data$clust4 <- clust_names[plot_data$clust4]
plot_data$clust4 <- factor(plot_data$clust4, 
  levels = clust_names, 
  labels = clust_names)

# Setup an example data frame
df <- data.frame(id=c("id1","id2","id3","id4","id5","id6","id7","id8"),
  val=c(0,1,0,2,3,1,2,NA))

# Now setup the translation vector - essentially a lookup table
trans <- c("ZZT","ZZU","ZZV","ZZW",NA)
names(trans) <- c(0,1,2,3,NA)

# Now translate the values into a new column and print it out 
df$nval <- trans[ as.character(df$val) ]
df$nval
# [1] "ZZT" "ZZU" "ZZT" "ZZV" "ZZW" "ZZU" "ZZV" NA 

# Plotcode lookup
plotcode_plot_group_lookup <- readRDS("data/plotcode_plot_group_lookup.rds")

# Original data
load("data/clean_input_data.Rdata")
load("data/seosaw_plot_summary5Apr2019.Rdata")

# Regional tmeperature and precipitation data for the Miombo woodlands
t_p <- readRDS("data/temp_precip.rds")

# Whites veg map
white_veg <- readOGR(dsn="/Users/johngodlee/google_drive/postgrad/phd/confirmation_report/resources/whitesveg", 
  layer="Whites vegetation")

white_veg_fort <- fortify(white_veg, region = "DESCRIPTIO")
names(white_veg_fort)
length(unique(white_veg_fort$id))

white_veg_miombo <- white_veg_fort %>%
  filter(id %in% c("Moist-infertile savanna"),
    lat < -2)

# Map of plot locations, compared to full SEOSAW dataset ----

# Create vector of southern Africa ISO codes - find a way to mine the data for this
s_af <- iso.expand(c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", 
  "MWI", "AGO", "TZA", "COG", "RWA", "BDI", "UGA", "KEN")) 

# Create map of country outlines
map_africa <- borders(database = "world", regions = s_af, fill = "grey90", colour = "black")
map_africa_fill <- borders(database = "world", regions = s_af, fill = "grey90")
map_africa_colour <- borders(database = "world", regions = s_af, colour = "black")

# Spatial distribution of key variables

var_map <- function(x, var, lab = expression("log(AGB) (t ha"^-1*")")){
  map_var <- ggplot() + 
    map_africa + 
    geom_point(data = x, aes(x = long, y = lat, fill = var), 
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
var_map(plot_data, 
  var = plot_data$agb_ha, 
  lab = expression("log(AGB) (t ha"^-1*")"))
dev.off()

# Precipitation
pdf(file = "img/precip_map.pdf", width = 5, height = 8)
var_map(plot_data, 
  var = plot_data$total_precip, 
  lab = expression("Mean" ~ "Annual" ~ "Precipitation" ~ (mm ~ y^-1)))
dev.off()

# Temp
pdf(file = "img/temp_map.pdf", width = 5, height = 8)
var_map(plot_data, 
  var = plot_data$mean_temp, 
  lab = expression("MAT" ~ (degree*C)))
dev.off()

# Soil Organic Carbon
pdf(file = "img/ocdens_map.pdf", width = 5, height = 8)
var_map(plot_data, 
  var = plot_data$ocdens, 
  lab = expression("Organic" ~ "C" ~ "(%)"))
dev.off()

# Biogeographic clusters facetted
pdf(file = "img/clust_map.pdf", width = 14, height = 6)
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = "Miombo\nwoodland"), 
    colour = "#A37803",
    data = white_veg_miombo, alpha = 1) +
  scale_fill_manual(name = "", values = "#A37803") +
  new_scale_fill() + 
  geom_point(data = plot_data, 
    aes(x = long, y = lat, fill = as.character(clust4)), 
    size = 2, shape = 21, colour = "black", position = "jitter") +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  map_africa_colour +
  facet_wrap(~clust4, nrow = 1) + 
  coord_map() + 
  ylim(-35.5, 10) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic() + 
  theme(legend.position = "none")
dev.off()

# How much of the climate space do the plots cover ----
# Standardize pixel values and convert to spatialpoints object
t_p_std <- data.frame(
  t_vals_std = as.vector(scale(t_p$t_vals)),
  p_vals_std = as.vector(scale(t_p$p_vals)))

t_p_std_nona <- na.omit(t_p_std)
t_p_nona <- na.omit(t_p)

t_p_sp <- SpatialPointsDataFrame(
  coords = t_p_std_nona, data = t_p_std_nona)

# Create a convex hull spatialpolygons object
plot_hull <- chull(plot_data$mean_temp, plot_data$total_precip)
plot_hull <- c(plot_hull, plot_hull[1])
plot_hull_data <- plot_data[plot_hull,]
plot_hull_coords <- plot_hull_data %>% dplyr::select(mean_temp, total_precip)

plot_hull_poly <- Polygon(plot_hull_coords, hole=F)
plot_hull_polys <- Polygons(list(plot_hull_poly), 1)
plot_hull_polys_sp = SpatialPolygons(list(plot_hull_polys))

plot_hull_std <- chull(plot_data$mean_temp_rev_std, plot_data$total_precip_std)
plot_hull_std <- c(plot_hull_std, plot_hull_std[1])
plot_hull_std_data<- plot_data[plot_hull_std,]
plot_hull_std_coords <- plot_hull_std_data %>% dplyr::select(mean_temp_rev_std, total_precip_std)

plot_hull_std_poly <- Polygon(plot_hull_std_coords, hole=F)
plot_hull_std_polys <- Polygons(list(plot_hull_std_poly), 1)
plot_hull_std_polys_sp = SpatialPolygons(list(plot_hull_std_polys))

# Count number of pixels covered by polygon
t_p_hull <- over(t_p_sp, plot_hull_polys_sp)

t_p_hull_std <- over(t_p_sp, plot_hull_std_polys_sp)

pixel_cover <- round(100 - length(t_p_hull_std[is.na(t_p_hull_std)]) / length(t_p_hull_std[!is.na(t_p_hull_std)]) * 100, 
  digits = 1)

fileConn <- file(paste0("include/hull_cover.tex"))
writeLines(
    paste0("\\newcommand{\\hullcover}{", pixel_cover, "}"),
  fileConn)
close(fileConn)

# Plot the convex hull and the points together
plot_hull_fort <- fortify(plot_hull_polys_sp)

pdf(file = "img/temp_precip_hull.pdf", width = 6, height = 6)
ggplot() + 
  stat_binhex(data = t_p, 
    mapping = aes(x = t_vals, y = p_vals, colour = ..count.., fill = ..count..),
    bins = 500, alpha = 0.2) + 
  scale_fill_continuous(name = "Density", type = "viridis", trans = "log",
    breaks = c(1, 5, 10, 50, 100, 200, 400)) + 
  scale_colour_continuous(name = "Density", type = "viridis", trans = "log", 
    breaks = c(1, 5, 10, 50, 100, 200, 400)) + 
  new_scale_fill() +
  geom_point(data = plot_data,
    mapping = aes(x = mean_temp, y = total_precip, fill = clust4), 
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
unique(plot_data$country)
length(unique(plot_data$country))

# How many plots did the original data have?
length(unique(ssaw8$plotInfoFull$plotcode))

# How many plots does the clean dataset have?
n_plots_num <- length(unique(plot_data$plot_group))

# Plot number pre-outlier detection
n_plots_pcoa <- n_plots_num + 110

fileConn <- file(paste0("include/n_plots.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\nplots}{", n_plots_num, "}"),
    paste0("\\newcommand{\\nplotspcoa}{", n_plots_pcoa, "}")),
  fileConn)
close(fileConn)

tail(sort(plot_data$total_precip), n = 20)
head(sort(plot_data$total_precip), n = 20)

# Where are the wettest plots found?
plot_data_precip_max_min <- plot_data %>%
  filter(total_precip > quantile(total_precip, 0.9) | 
      total_precip < quantile(total_precip, 0.1)) %>%
  mutate(hi_lo = case_when(
    total_precip > median(plot_data$total_precip) ~ "hi",
    total_precip < median(plot_data$total_precip) ~ "lo"
  ))

pdf(file = "img/precip_extremes_map.pdf", width = 5, height = 8)
ggplot() + 
  map_africa_fill + 
  geom_polygon(aes(x = long, y = lat, group = group), 
    fill = "#16CC7D", colour = "#16CC7D",
    data = white_veg_miombo, alpha = 1) +
  geom_point(data = plot_data_precip_max_min, 
    aes(x = long, y = lat, fill = hi_lo), 
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
sum(plot_data$n_stems)

# Diversity variables
plot_data_gather_div <- plot_data %>%
  dplyr::select(clust4, agb_ha_log, mean_height, cov_height,
    cov_dbh, stems_ha_log, n_species_raref_log) %>%
  gather(key = "variable", value = "value", 3:7)

pdf(file = "img/biomass_div_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_gather_div, aes(x = value, y = agb_ha_log)) + 
  geom_point(aes(fill = clust4), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust4)) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  facet_wrap(~variable, scales = "free_x", labeller = label_parsed)
dev.off()

# How much AGB is in small stems
##' Small stems = those not included in analyses
s_small <- s %>%
  filter(diam < 10) %>%
  group_by(plotcode) %>%
  summarise(agb_small = sum(Bchave))

# proportion of total agb
s_all <- s %>%
  group_by(plotcode) %>%
  summarise(agb_all = sum(Bchave)) %>%
  left_join(s_small, ., by = "plotcode") %>%
  mutate(perc_small = agb_small / agb_all * 100) %>%
  filter(perc_small < 30) %>%
  arrange(desc(perc_small))

perc_small_agb <- round(median(s_all$perc_small), digits = 1)

fileConn <- file(paste0("include/perc_small_agb.tex"))
writeLines(
  paste0("\\newcommand{\\percsmallagb}{", perc_small_agb, "}"),
  fileConn)
close(fileConn)

# Which species make up the vegetation clusters? ----

# Create community composition matrix of suitable plots
s_fil <- s %>%
  left_join(., plotcode_plot_group_lookup, by = c("plotcode" = "plotcode_vec")) %>%
  inner_join(., data.frame(plot_group = plot_data$plot_group), 
    by = c("plot_group" = "plot_group")) %>%
  filter(alive %in% c("A", NA),
    !is.na(gen_sp)) %>%
  left_join(., select(plot_data, plot_group, clust4), by = "plot_group")

# Create community composition matrix
ab_mat <- s_fil %>%
  group_by(plot_group, gen_sp, .drop = FALSE) %>%
  tally() %>%
  spread(gen_sp, n, fill = 0) %>%
  ungroup() %>%
  data.frame() %>%
  select(-plot_group) %>%
  select_if(any)

# Remove species not present in any plot
clust4_vec <- plot_data$clust4

# Run Dufrene-Legendre indicator species analysis
clust_indval <- indval(ab_mat, clustering = clust4_vec)

sink("output/clust_species_indicators.txt")
summary(clust_indval, p = 0.05, type = "short", digits = 2, show = p)
sink()

# Extract indicator species per cluster
clust_indval$indval$sp <- row.names(clust_indval$indval)
row.names(clust_indval$indval) <- seq(from = 1, to = length(clust_indval$indval$sp))

for(i in 1:4){
  print(head(clust_indval$indval[order(clust_indval$indval[[i]], decreasing = TRUE),c(5, i)]))
}

fileConn<-file("output/clust_species_ind_summary.txt")
writeLines(c(
  "Cluster 1 - Marginal miombo",
  "Diplorhynchus.condylocarpon, Burkea.africana, Pseudolachnostylis.maprouneifolia",
  "", 
  "Cluster 2 - Core miombo",
  "Julbernardia.paniculata, Isoberlinia.angolensis, Pterocarpus.angolensis, Brachystegia.longifolia",
  "",
  "Cluster 3 - Baikiaea",
  "Baikiaea.plurijuga, Senegalia.ataxacantha, Terminalia.sericea",
  "",
  "Cluster 4 - Mopane",
  "Colophospermum mopane"
), fileConn)
close(fileConn)

# Find dominant species by biomass per cluster
clust_dom <- s_fil %>%
  group_by(clust4, gen_sp) %>%
  summarise(bchave_total = sum(Bchave))

clust_dom_list <- split(clust_dom, clust_dom$clust4)

for(i in 1:4){
  print(head(clust_dom_list[[i]][order(clust_dom_list[[i]]$bchave_total, decreasing = TRUE),]))
}

fileConn <- file("output/clust_species_dom_summary.txt")
writeLines(c(
  "Cluster 1 - Marginal miombo",
  "Julbernadia.spp., Brachystegia.spiciformis, Baikiaea.plurijuga",
  "",
  "Cluster 2 - Core miombo",
  "Julbernadia.spp., Brachystegia.spp., Isoberlinia.angolensis",
  "",
  "Cluster 3 - Baikiaea",
  "Spirostachys.africana, Senegalia.spp., Euclea.racemosa",
  "",
  "Cluster 4 - Mopane",
  "Colophospermum.mopane"
), fileConn)
close(fileConn)

# Build a tidy table describing vegetation clusters
c_name <- c("Marginal miombo", "Core miombo", "Baikiaea", "Mopane")
c_ind <- c(
  "Diplorhynchus condylocarpon, Burkea africana, Pseudolachnostylis maprouneifolia",
  "Julbernardia paniculata, Isoberlinia angolensis, Brachystegia longifolia",
  "Baikiaea plurijuga, Senegalia ataxacantha, Combretum collinum",
  "Colophospermum mopane, Combretum spp.")
c_dom <- c(
  "Julbernadia spp., Brachystegia spiciformis, Baikeaea plurijuga",
  "Julbernadia spp., Brachystegia spp., Isoberlinia angolensis",
  "Spirostachys africana, Senegalia spp., Euclea racemosa",
  "Colophospermum mopane")

clust_summ <- plot_data %>%
  group_by(clust4) %>%
  summarise(
    n_plots = n(),
    agb_ha_min = round(min(agb_ha, na.rm = TRUE), digits = 3),
    agb_ha_max = round(max(agb_ha, na.rm = TRUE), digits = 3),
    agb_ha_median = round(mean(agb_ha, na.rm = TRUE), digits = 1),
    agb_ha_sd = round(sd(agb_ha, na.rm = TRUE), digits =  2),
    agb_ha_iqr = round(IQR(agb_ha, na.rm = TRUE), digits = 2),
    
    n_species_raref_median = round(mean(n_species_raref, na.rm = TRUE), digits = 0),
    n_species_raref_sd = round(sd(n_species_raref, na.rm = TRUE), digits = 1),
    n_species_raref_iqr = round(IQR(n_species_raref, na.rm = TRUE), digits = 1),
    
    stems_ha_median = round(mean(stems_ha, na.rm = TRUE), digits = 0),
    stems_ha_iqr = round(IQR(stems_ha, na.rm = TRUE), digits = 1),
    stems_ha_sd = round(sd(stems_ha, na.rm = TRUE), digits = 1)) %>%
  mutate(c_name, c_ind, c_dom,
    agb_ha = paste0(agb_ha_median, "(", agb_ha_iqr, ")"),
    n_species_raref = paste0(n_species_raref_median, "(", n_species_raref_iqr, ")"),
    stems_ha = paste0(stems_ha_median, "(", stems_ha_iqr, ")")) %>%
  select(clust4, c_dom, c_ind,  
    n_plots, n_species_raref, stems_ha, agb_ha)

fileConn <- file("include/clust_summ.tex")
writeLines(stargazer(clust_summ, 
  summary = FALSE,
  label = "clust_summ", digit.separate = 0, rownames = FALSE), fileConn)
close(fileConn)
