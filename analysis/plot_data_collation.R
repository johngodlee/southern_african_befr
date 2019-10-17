# Creating a dataset for use in Chapter 1 - Regional analysis of the factors affecting woody AGB and productivity
# John Godlee (johngodlee@gmail.com)
# 2018_12_10

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
library(reshape2)
library(vegan)
library(gridExtra)

# Import data ----

# Base SEOSAW plot summary dataset
load("data/seosaw_plot_summary5Apr2019.Rdata")
##' AGB
##' Shannon / Sp. R
##' Stem density
##' Soil nutrients (CEC)
##' Plot location

# Per stem data 
load("data/clean_input_data.Rdata")
##' Coefficient of variation height
##' Coefficient of variation DBH

# Aridity data 
aridity_index <- read.csv("data/seosaw_aridity_index.csv")
##' Aridity index from WorldClim calculations

# Soil organic carbon
ocdens <- read.csv("data/seosaw_ocdens.csv")

# MODIS fire data 
fire_return <- read.csv("data/fire_return.csv")
##' Data copied from burn /exports/...

# WorldClim temp.
temp <- read.csv("data/plot_temp.csv")

# WorldClim precip.
precip <- read.csv("data/plot_precip.csv")

# Clean data ----

# Create large raw dataframe
plot_data <- ssaw8$struct %>%
  left_join(., ssaw8$structPerHa, by = c("plotcode" = "plotcode")) %>%
  left_join(., ssaw8$plotInfoFull, by = c("plotcode" = "plotcode")) %>%
  left_join(., ssaw8$floristicsGT5, by = c("plotcode" = "plotcode")) %>%
  left_join(., ssaw8$soil, by = c("plotcode" = "plotcode")) %>%
  left_join(., ssaw8$climate, by = c("plotcode" = "plotcode")) %>%
  left_join(., ssaw8$env, by = c("plotcode" = "plotcode")) %>%
  left_join(., ssaw8$cluster, by = c("plotcode" = "plotcode")) %>%
  left_join(., aridity_index, by = c("plotcode" = "plotcode")) %>%
  left_join(., ocdens, by = c("plotcode" = "plotcode")) %>%
  left_join(., fire_return, by = c("plotcode" = "plotcode")) %>%  
  left_join(., temp, by = c("plotcode" = "plotcode")) %>%
  left_join(., precip, by = c("plotcode" = "plotcode")) %>%
  dplyr::select(plotcode,
    longitude_of_centre = longitude_of_centre.x.x,
    latitude_of_centre = latitude_of_centre.x.x,
    n = n_stems.x,
    bchave = Bchave,
    country = country.x,
    area_of_plot = area_of_plot.x, 
    shape_of_plot, 
    manipulation_experiment, 
    timber_harvesting_occurring, 
    was_the_plot_high_graded_in_last_100_years, 
    fuel_wood_harvesting, charcoal_harvesting, 
    other_woody_product_harvesting, ntfp_harvesting,
    used_for_farming_in_last_30_years, 
    protected_from_fire, elephant_exclosure, 
    cattle_graze_here, goats_graze_here,
    sp_rich = div.S,
    shannon = div.J,
    cation_ex_cap = CECSOL_M_sl1_1km_ll,
    sand_per = SNDPPT_M_sl1_1km_ll,
    org_c_per = ORCDRC_M_sl1_1km_ll,
    aridity_index = ai,
    total_precip = bio12,
    wc_total_precip = p_plot,
    precip_seasonality = bio15,
    wc_precip_seasonality = p_cov_plot,
    ocdens = ocdens,
    mean_temp = bio1,
    wc_mean_temp = t_plot,
    temp_seasonality = bio4,
    wc_temp_seasonality = t_cov_plot,
    pi, 
    plot_id,
    fire_return_mean,
    firecount_2001_2018,
    clust7, clust5, clust4)

# Compare SEOSAW worldclim temp with worldclim from my extraction
ggplot(plot_data, aes(x = mean_temp, y = wc_mean_temp)) + 
  geom_point()

ggplot(plot_data, aes(x = temp_seasonality, y = wc_temp_seasonality)) + 
  geom_point()

# Compare SEOSAW worldclim precip with worldclim from my extraction
ggplot(plot_data, aes(x = total_precip, y = wc_total_precip)) + 
  geom_point()

ggplot(plot_data, aes(x = precip_seasonality, y = wc_precip_seasonality)) + 
  geom_point()

# Keep WC estimates from here on
plot_data <- plot_data %>%
  dplyr::select(-total_precip, -mean_temp, -temp_seasonality, -precip_seasonality) %>%
  rename(total_precip = wc_total_precip,
    precip_seasonality = wc_precip_seasonality,
    mean_temp = wc_mean_temp,
    temp_seasonality = wc_temp_seasonality)

# Aggregate Zambian Forestry Commission plots ----

# Split into Zambia and non-Zambia datasets
plot_data_zam <- plot_data %>%
  filter(pi == "Siampale")
  
plot_data_nozam <- plot_data %>%
  filter(pi != "Siampale")

# Aggregate values for Zambian plots and randomly sample rows
plot_data_zam_agg <- plot_data_zam %>%
  separate(plot_id, c("plot_group", "plot_subset"), remove = FALSE) %>%
  group_by(plot_group) %>%
  summarise(plotcode = paste0(plotcode, collapse = ","),
    longitude_of_centre = mean(longitude_of_centre, na.rm = TRUE),
    latitude_of_centre = mean(latitude_of_centre, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    bchave = mean(bchave, na.rm = TRUE),
    country = first(na.omit(country)),
    area_of_plot = sum(area_of_plot, na.rm = TRUE),
    shape_of_plot = first(na.omit(shape_of_plot)),
    manipulation_experiment = first(na.omit(manipulation_experiment)),
    timber_harvesting_occurring = first(na.omit(timber_harvesting_occurring)),
    was_the_plot_high_graded_in_last_100_years = first(na.omit(was_the_plot_high_graded_in_last_100_years)),
    fuel_wood_harvesting = first(na.omit(fuel_wood_harvesting)),
    charcoal_harvesting = first(na.omit(charcoal_harvesting)),
    other_woody_product_harvesting = first(na.omit(other_woody_product_harvesting)),
    ntfp_harvesting = first(na.omit(ntfp_harvesting)),
    used_for_farming_in_last_30_years = first(na.omit(used_for_farming_in_last_30_years)),
    protected_from_fire = first(na.omit(protected_from_fire)), 
    elephant_exclosure = first(na.omit(elephant_exclosure)),
    cattle_graze_here = first(na.omit(cattle_graze_here)),
    goats_graze_here = first(na.omit(goats_graze_here)),
    sp_rich = trunc(mean(sp_rich, na.rm = TRUE)),
    shannon = mean(shannon, na.rm = TRUE),
    cation_ex_cap = mean(cation_ex_cap, na.rm = TRUE),
    sand_per = mean(sand_per, na.rm = TRUE),
    org_c_per = mean(org_c_per, na.rm = TRUE),
    aridity_index = mean(aridity_index, na.rm = TRUE),
    total_precip = mean(total_precip, na.rm = TRUE),
    precip_seasonality = mean(precip_seasonality, na.rm = TRUE),
    ocdens = mean(ocdens, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    temp_seasonality = mean(temp_seasonality, na.rm = TRUE),
    pi = first(na.omit(pi)),
    plot_id = first(na.omit(plot_id)),
    fire_return_mean = mean(fire_return_mean, na.rm = TRUE),
    firecount_2001_2018 = first(na.omit(firecount_2001_2018)),
    clust7 = first(na.omit(clust7)), 
    clust5 = first(na.omit(clust5)),
    clust4 = first(na.omit(clust4))) %>%
  sample_n(500)

# Plot variation in species richness and rarefied sp_r following aggregation ----

# Combine full and aggregated data
plot_data_zam$full_agg <- "full"
plot_data_zam_agg$full_agg <- "agg"

plot_zam_agg_noagg <- rbind(plot_data_zam, 
  dplyr::select(plot_data_zam_agg, -plot_group))

# Box plot of species richness
rich_box <- ggplot() + 
  geom_violin(data = plot_zam_agg_noagg, aes(x = full_agg, y = sp_rich, fill = full_agg), alpha = 0.5) + 
  geom_boxplot(data = plot_zam_agg_noagg, aes(x = full_agg, y = sp_rich, fill = full_agg), alpha = 0.5) + 
  scale_fill_manual(values = c("red", "blue")) + 
  theme_classic() + 
  labs(x = "Aggregated?", y = "Species richness") + 
  ggtitle("Species richness") + 
  theme(legend.position = "none")

# Create matrix for estimating rarefied species richness 
ab_mat_zam <- s %>% 
  filter(plotcode %in% plot_zam_agg_noagg$plotcode) %>%
  dplyr::select(plotcode, gen_sp) %>%
  dcast(., plotcode~gen_sp)

row.names(ab_mat_zam) <- ab_mat_zam$plotcode

ab_mat_zam <- dplyr::select(ab_mat_zam, -plotcode)

ab_mat_zam_agg <- s %>% 
  filter(plotcode %in% plot_zam_agg_noagg$plotcode) %>%
  separate(plot_id, c("plot_group", "plot_subset"), remove = FALSE) %>%
  dplyr::select(plot_group, gen_sp) %>%
  dcast(., plot_group~gen_sp)

row.names(ab_mat_zam_agg) <- ab_mat_zam_agg$plot_group

ab_mat_zam_agg <- dplyr::select(ab_mat_zam_agg, -plot_group)

# Calculate rarefied species richness
raref <- as.data.frame(rarefy(ab_mat_zam, sample = 20, se = FALSE))
raref$plotcode <- row.names(raref)
names(raref) <- c("sp_rich_raref", "plotcode")
raref$full_agg <- "full"
raref_agg <- as.data.frame(rarefy(ab_mat_zam_agg, sample = 20, se = FALSE))
raref_agg$plotcode <- row.names(raref_agg)
names(raref_agg) <- c("sp_rich_raref", "plotcode")
raref_agg$full_agg <- "agg"

raref_all <- rbind(raref, raref_agg)

# Boxplot of rarefied species richness
raref_box <- ggplot() + 
  geom_violin(data = raref_all, aes(x = full_agg, y = sp_rich_raref, fill = full_agg), alpha = 0.5) + 
  geom_boxplot(data = raref_all, aes(x = full_agg, y = sp_rich_raref, fill = full_agg), alpha = 0.5) + 
  scale_fill_manual(values = c("red", "blue")) + 
  theme_classic() + 
  labs(x = "Aggregated?", y = "Species richness") + 
  ggtitle("Rarefied species richness") + 
  theme(legend.position = "none")

# Arrange both boxplots and save
pdf(file = "img/sp_rich_raref_agg.pdf", width = 12, height = 7)
grid.arrange(rich_box, raref_box, ncol = 2) 
dev.off()

# Combine Zambian aggregated data with non-aggregated other data ----
plot_data_agg <- bind_rows(plot_data_zam_agg, plot_data_nozam) %>%
  mutate(plot_group = if_else(!is.na(plot_group), plot_group, plotcode))

# Remove small plots
plot_data_agg <- plot_data_agg %>%
  filter(area_of_plot >= 0.1)
##' remove 501 plots

# Add plot_group identifier in stem level data ----
plot_data_agg$plotcode_vec <- strsplit(as.character(plot_data_agg$plotcode), split=",")

plotcode_plot_group_lookup <- plot_data_agg %>%
  dplyr::select(plotcode_vec, plot_group) %>%
  unnest(plotcode_vec)

write.csv(plotcode_plot_group_lookup, "data/plotcode_plot_group_lookup.csv", row.names = FALSE)

s <- left_join(s, plotcode_plot_group_lookup, by = c("plotcode" = "plotcode_vec"))
##' No loss of data

# Filter stem data to only include large stems, then calculate dbh and height covariance ----
s_fil <- s %>%
  filter(!is.na(plot_group), 
    diam >= 5) 

s_fil_summ <- s_fil %>%
  group_by(plot_group) %>%
  summarise(n_stems = n(),
    var_height = var(height, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    sd_height = sd(height, na.rm = TRUE),
    var_dbh = var(diam, na.rm = TRUE),
    mean_dbh = mean(diam, na.rm = TRUE),
    sd_dbh = sd(diam, na.rm = TRUE)) %>%
  mutate(cov_height = sd_height / mean_height * 100,
    cov_dbh = sd_dbh / mean_dbh * 100)

# Join number of stems to full dataset, removing plots with no estimates
plot_data_agg <- left_join(plot_data_agg, s_fil_summ, by = c("plot_group" = "plot_group"))

# Calculate stems per hectare ----
plot_data_agg$stems_ha <- plot_data_agg$n_stems / plot_data_agg$area_of_plot

# Remove plots with fewer than 10 stems per hectare ----
plot_data_agg <- plot_data_agg %>%
  filter(stems_ha >= 10)

# Remove plots with fewer than 10 stems total ----
plot_data_agg <- plot_data_agg %>% 
  filter(n_stems >= 15)

# Remove plots with management, experiments, grazing etc. ----
plot_data_agg <- plot_data_agg %>%
  filter(
    charcoal_harvesting %in% c("N", "No", "No (its not common here)", NA, ""), 
    timber_harvesting_occurring %in% c(NA, "N", "No (no pecies of commercial value)", "", "No"),
    was_the_plot_high_graded_in_last_100_years %in% c("N", "No", "Unknown", "", NA),
    manipulation_experiment %in% c("N", NA, "", "n"),
    fuel_wood_harvesting %in% c(NA, "N", "yes deadwood", "", "No"),
    other_woody_product_harvesting %in% c(NA, "N", "No", ""),
    used_for_farming_in_last_30_years %in% c(NA, "N", "No (becaues of slope mostly its hilly, but in low lying areas farming is practised)", "", "?"),
    protected_from_fire %in% c(NA, "N", "No", "", "not intentionally"),
    cattle_graze_here %in% c(NA, "N", "Yes but mainly along the boundaries/edge", "", "No", "N "),
    goats_graze_here %in% c(NA, "N", "Yes bu mainly along the boundaries/edge", "", "No"),
    !is.na(clust5)) %>%
  dplyr::select(-shape_of_plot, -manipulation_experiment, 
    -timber_harvesting_occurring, 
    -was_the_plot_high_graded_in_last_100_years, 
    -fuel_wood_harvesting, 
    -charcoal_harvesting, 
    -other_woody_product_harvesting, 
    -ntfp_harvesting, 
    -used_for_farming_in_last_30_years, 
    -protected_from_fire, 
    -elephant_exclosure, 
    -cattle_graze_here, 
    -goats_graze_here,
    -plotcode_vec)

# Remove plot with a crazy number of species
##' The 10 Ha plot in DRC
plot_data_agg <- plot_data_agg %>% 
  filter(sp_rich != max(plot_data_agg$sp_rich))

# Estimate rarefied species richness
ab_mat <- s_fil %>% 
  filter(plot_group %in% plot_data_agg$plot_group) %>%
  dplyr::select(plot_group, gen_sp) %>%
  dcast(., plot_group~gen_sp)

row.names(ab_mat) <- ab_mat$plot_group

ab_mat <- dplyr::select(ab_mat, -plot_group)

rarecurve_list <- rarecurve(ab_mat, step = 1, label = FALSE)

## GGPLOT ##

raref <- as.data.frame(rarefy(ab_mat, sample = 15, se = TRUE))
raref_df <- data.frame(t(raref))
raref_df$plot_group <- c(row.names(raref_df))
names(raref_df) <- c("sp_rich_raref", "sp_rich_raref_sd", "plot_group")

plot_data_agg <- left_join(plot_data_agg, raref_df, by = "plot_group")

# Estimate Species abundance evenness from Shannon
plot_data_agg$shannon_equit <- plot_data_agg$shannon / plot_data_agg$sp_rich

# Create and index of fire intensity to account for plots with no fire
##' Currently only 684 plots have fire
plot_data_agg$fire_index <- as.character(cut_number(plot_data_agg$fire_return_mean, n = 4, 
  labels = c("Frequent", "Occassional", "Rare", "Very rare")))
plot_data_agg$fire_index[is.na(plot_data_agg$fire_index)] <- "No fire"
plot_data_agg$fire_index <- factor(plot_data_agg$fire_index, levels = c("Frequent", "Occassional", "Rare", "Very rare", "No fire"))

# Ensure that variables all have the same sign
plot_data_agg$precip_seasonality <- 1 / plot_data_agg$precip_seasonality * 10000
plot_data_agg$temp_seasonality <- 1 / plot_data_agg$temp_seasonality * 10000
plot_data_agg$sand_per <- 1 / plot_data_agg$temp_seasonality * 10000

# Write to CSV
write.csv(plot_data_agg, "data/plot_data_fil_agg.csv", row.names = FALSE)
  
