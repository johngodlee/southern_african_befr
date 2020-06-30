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
library(stargazer)  
library(labdsv)  

source("scripts/clust_defin.R")

# Import data ----

# Cleaned data
plot_data <- readRDS("data/plot_data_fil_agg_norm_std.rds")

# SEOSAW v2 stems data
stems <- read.csv("~/git_proj/seosaw_data/data_out/stems_latest_v2.7.csv")

# Tree abundance matrix
ab_mat <- readRDS("data/stems_ab_mat.rds")

# plot_id plot_cluster lookup
plotcode_plot_cluster_lookup <- readRDS("data/plotcode_plot_cluster_lookup.rds")

# Regional tmeperature and precipitation data for the Miombo woodlands
t_p <- readRDS("data/temp_precip.rds")

# Whites veg map
white_veg <- readOGR(dsn="/Volumes/john/whitesveg", 
  layer="whitesveg")

# Fortify whitesveg and subset to Miombo ----
white_veg_fort <- fortify(white_veg, region = "DESCRIPTIO")

white_veg_miombo <- white_veg_fort %>%
  filter(id %in% c("Moist-infertile savanna"),
    lat < -2)

# Make clust4 as factor ----
plot_data$clust4 <- clust_names[plot_data$clust4]
plot_data$clust4 <- factor(plot_data$clust4)

# Map of plot locations, compared to full SEOSAW dataset ----

# Create vector of southern Africa ISO codes  
s_af <- iso.expand(c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", 
  "MWI", "AGO", "TZA", "COG", "RWA", "BDI", "UGA", "KEN", "SWZ")) 

# Create map of country outlines
map_africa <- borders(database = "world", regions = s_af, fill = "grey90", colour = "black")
map_africa_fill <- borders(database = "world", regions = s_af, fill = "grey90")
map_africa_colour <- borders(database = "world", regions = s_af, colour = "black")

# Biogeographic clusters facetted
pdf(file = "img/clust_map.pdf", width = 14, height = 6)
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = "Miombo\nwoodland"), 
    colour = "#A37803",
    data = white_veg_miombo, alpha = 1) +
  scale_fill_manual(name = "", values = "#A37803") +
  new_scale_fill() + 
  geom_point(data = plot_data, 
    aes(x = longitude_of_centre, y = latitude_of_centre, fill = as.character(clust4)), 
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

# How much climate space do the plots cover ----
# Standardize pixel values and convert to spatialpoints object
t_p_std <- data.frame(
  t_vals_std = as.vector(scale(t_p$t_vals)),
  p_vals_std = as.vector(scale(t_p$p_vals)))

t_p_std_nona <- na.omit(t_p_std)
t_p_nona <- na.omit(t_p)

t_p_sp <- SpatialPointsDataFrame(
  coords = t_p_std_nona, data = t_p_std_nona)

# Create a convex hull spatialpolygons object
plot_hull <- chull(plot_data$temp, plot_data$precip)
plot_hull <- c(plot_hull, plot_hull[1])
plot_hull_data <- plot_data[plot_hull,]
plot_hull_coords <- plot_hull_data %>% 
  dplyr::select(temp, precip)

plot_hull_poly <- Polygon(plot_hull_coords, hole=FALSE)
plot_hull_polys <- Polygons(list(plot_hull_poly), 1)
plot_hull_polys_sp = SpatialPolygons(list(plot_hull_polys))

plot_hull_std <- chull(plot_data$temp_std, plot_data$precip_std)
plot_hull_std <- c(plot_hull_std, plot_hull_std[1])
plot_hull_std_data<- plot_data[plot_hull_std,]
plot_hull_std_coords <- plot_hull_std_data %>% 
  dplyr::select(temp_std, precip_std)

plot_hull_std_poly <- Polygon(plot_hull_std_coords, hole=FALSE)
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
    mapping = aes(x = temp, y = precip, fill = clust4), 
    colour = "black", shape = 21) + 
  geom_polygon(data = plot_hull_fort,
    aes(x = long, y = lat), fill = NA, colour = "#A3152A") + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  labs(x= expression("MAT" ~ (degree*C)), 
    y = expression("MAP" ~ (mm ~ y^-1))) + 
  theme(aspect.ratio = 1) 
dev.off()

# How many plots does the clean dataset have?
n_plots_num <- length(unique(plot_data$plot_cluster))

# How many trees did we measure?
n_trees_total <- round(sum(rowSums(ab_mat[,!(names(ab_mat) == "plot_cluster")])), 0)

fileConn <- file(paste0("include/n_plots.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\nplots}{", n_plots_num, "}"),
    paste0("\\newcommand{\\ntrees}{", n_trees_total, "}")),
  fileConn)
close(fileConn)

tail(sort(plot_data$precip), n = 20)
head(sort(plot_data$precip), n = 20)

# Where are the wettest plots found?
plot_data_precip_max_min <- plot_data %>%
  filter(precip > quantile(precip, 0.9) | 
      precip < quantile(precip, 0.1)) %>%
  mutate(hi_lo = case_when(
    precip > median(plot_data$precip) ~ "hi",
    precip < median(plot_data$precip) ~ "lo"
  ))

# How much AGB is in small stems
##' Small stems = those not included in analyses
s_small <- stems %>%
  filter(diam < 10) %>%
  group_by(plot_id) %>%
  summarise(agb_small = sum(agb))

# proportion of total agb
s_all <- stems %>%
  group_by(plot_id) %>%
  summarise(agb_all = sum(agb)) %>%
  left_join(s_small, ., by = "plot_id") %>%
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

# Run Dufrene-Legendre indicator species analysis
clust_indval <- indval(ab_mat, clustering = plot_data$clust4)

sink("output/clust_species_indicators.txt")
summary(clust_indval, p = 0.05, type = "short", digits = 2, show = p)
sink()

# Extract indicator species per cluster
clust_indval$indval$sp <- row.names(clust_indval$indval)
row.names(clust_indval$indval) <- seq(from = 1, to = length(clust_indval$indval$sp))

indval_extrac <- lapply(1:4, function(x) {
    out <- head(clust_indval$indval[order(clust_indval$indval[[x]], 
          decreasing = TRUE),c(5, x)])
    out[!grepl("indet", out$sp, ignore.case = TRUE),]
  })

indval_extrac_names <- sapply(indval_extrac, function(x) {names(x)[2]})

indval_extrac_order <- indval_extrac[match(indval_extrac_names, clust_names)]

indval_extrac_tidy <- unlist(lapply(1:4, function(x) {
  c(clust_names[x],
  paste(gsub(" ", ".", indval_extrac_order[[x]]$sp[1:3]), collapse = ", "),
  "")
}))

fileConn<-file("output/clust_species_ind_summary.txt")
writeLines(indval_extrac_tidy, fileConn)
close(fileConn)

# Find dominant species by biomass per plot per cluster
clust_dom <- stems %>%
  inner_join(., plotcode_plot_cluster_lookup, by = c("plot_id" = "plot_id_vec")) %>%
  inner_join(., plot_data, by = "plot_cluster") %>%
  mutate(bchave_prop = agb.x / agb.y) %>%
  group_by(clust4, species_name_clean) %>%
  summarise(bchave_prop_total = sum(bchave_prop)) %>%
  dplyr::select(-clust4)

clust_dom_list <- split(clust_dom, clust_dom$clust4)

dom_extrac <- lapply(1:4, function(x) {
  out <- head(clust_dom_list[[x]][order(clust_dom_list[[x]]$bchave_prop_total, 
        decreasing = TRUE),])
  out[!grepl("indet", out$species_name_clean, ignore.case = TRUE),]
})

dom_extrac_names <- as.character(sapply(dom_extrac, function(x) {
    unname(unlist(x[1,1]))
  }))

dom_extrac_order <- dom_extrac[match(dom_extrac_names, clust_names)]

dom_extrac_tidy <- unlist(lapply(1:4, function(x) {
  c(clust_names[x],
    paste(gsub(" ", ".", dom_extrac_order[[x]]$species_name_clean[1:3]), collapse = ", "),
    "")
  }))

fileConn <- file("output/clust_species_dom_summary.txt")
  writeLines(dom_extrac_tidy, fileConn)
close(fileConn)

# Build a tidy table describing vegetation clusters
c_ind <- gsub("\\.", " ", indval_extrac_tidy[c(2,5,8,11)])
c_dom <- gsub("\\.", " ", dom_extrac_tidy[c(2,5,8,11)])

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
    
    stems_ha_median = round(mean(n_trees_gt10_ha, na.rm = TRUE), digits = 0),
    stems_ha_iqr = round(IQR(n_trees_gt10_ha, na.rm = TRUE), digits = 1),
    stems_ha_sd = round(sd(n_trees_gt10_ha, na.rm = TRUE), digits = 1)) %>%
  mutate(c_name = clust_names, c_ind, c_dom,
    agb_ha = paste0(agb_ha_median, "(", agb_ha_iqr, ")"),
    n_species_raref = paste0(n_species_raref_median, "(", n_species_raref_iqr, ")"),
    stems_ha = paste0(stems_ha_median, "(", stems_ha_iqr, ")"),
    clust4 = as.character(clust4)) %>%
  select(clust4, c_dom, c_ind,  
    n_plots, n_species_raref, stems_ha, agb_ha)

fileConn <- file("include/clust_summ.tex")
writeLines(stargazer(clust_summ, 
  summary = FALSE,
  label = "clust_summ", digit.separate = 0, rownames = FALSE), fileConn)
close(fileConn)
