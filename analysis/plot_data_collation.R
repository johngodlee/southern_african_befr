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
library(tidyr)
library(iNEXT)
library(vegan)

# Import data ----

source("full_best.R")

# Base SEOSAW plot summary dataset
load("data/seosaw_plot_summary5Apr2019.Rdata")

# Per stem data 
load("data/clean_input_data.Rdata")

# Aridity data 
aridity_index <- read.csv("data/seosaw_aridity_index.csv")

# Soil organic carbon
ocdens <- read.csv("data/seosaw_ocdens.csv")

# MODIS fire data 
fire_return <- read.csv("data/fire_return.csv")

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
    cation_ex_cap = CECSOL_M_sl1_1km_ll,
    sand_per = SNDPPT_M_sl1_1km_ll,
    org_c_per = ORCDRC_M_sl1_1km_ll,
    aridity_index = ai,
    total_precip = p_plot,
    precip_seasonality = p_cov_plot,
    ocdens = ocdens,
    mean_temp = t_plot,
    isothermality = bio3,
    temp_seasonality = t_cov_plot,
    pi, 
    plot_id,
    fire_return_mean,
    firecount_2001_2018,
    clust7, clust5, clust4) %>%
  filter(!is.na(sp_rich),
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
    !is.na(clust5))

# Aggregate Zambian Forestry Commission plots ----

# Split into Zambia and non-Zambia datasets
plot_data_zam <- plot_data %>%
  filter(pi == "Siampale")
  
plot_data_nozam <- plot_data %>%
  filter(pi != "Siampale")

# Aggregate values for Zambian plots and randomly sample rows
plot_data_zam_agg <- plot_data_zam %>%
  separate(plot_id, c("plot_group", "plot_subset"), remove = FALSE) %>%
  mutate(plot_group = paste0(plot_group, "_zambia")) %>%
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
    sp_rich = round(mean(sp_rich, na.rm = TRUE)),
    cation_ex_cap = mean(cation_ex_cap, na.rm = TRUE),
    sand_per = mean(sand_per, na.rm = TRUE),
    org_c_per = mean(org_c_per, na.rm = TRUE),
    aridity_index = mean(aridity_index, na.rm = TRUE),
    total_precip = mean(total_precip, na.rm = TRUE),
    precip_seasonality = mean(precip_seasonality, na.rm = TRUE),
    ocdens = mean(ocdens, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    temp_seasonality = mean(temp_seasonality, na.rm = TRUE),
    isothermality = mean(isothermality, na.rm = TRUE),
    pi = first(na.omit(pi)),
    plot_id = first(na.omit(plot_id)),
    fire_return_mean = mean(fire_return_mean, na.rm = TRUE),
    firecount_2001_2018 = first(na.omit(firecount_2001_2018)),
    clust7 = first(na.omit(clust7)), 
    clust5 = first(na.omit(clust5)),
    clust4 = first(na.omit(clust4))) 

# Combine Zambian aggregated data with non-aggregated other data ----
plot_data_agg <- bind_rows(plot_data_zam_agg, plot_data_nozam) %>%
  mutate(plot_group = if_else(!is.na(plot_group), plot_group, plotcode))

# Add plot_group identifier in stem level data ----
plot_data_agg$plotcode_vec <- strsplit(as.character(plot_data_agg$plotcode), split=",")

plotcode_plot_group_lookup <- plot_data_agg %>%
  dplyr::select(plotcode_vec, plot_group) %>%
  unnest(plotcode_vec)

s <- left_join(s, plotcode_plot_group_lookup, by = c("plotcode" = "plotcode_vec"))

s_diam_summ <- s %>% 
  group_by(plot_group) %>%
  summarise(min_diam = min(diam, na.rm = TRUE)) %>%
  filter(min_diam < 10)

# Calculate dbh and height covariance ----
# Big trees only 
s_fil <- s %>%
  filter(!is.na(plot_group), 
    diam >= 5) %>%
  filter(alive %in% c("A", NA),
    !is.na(gen_sp))

dbh_class_lo <- as.character(seq(from = 5, to = 180, by = 5))
dbh_class_hi <- as.character(seq(from = 9, to = 184, by = 5))

dbh_labels <- paste0(dbh_class_lo, "-", dbh_class_hi)

s_fil_summ <- s_fil %>%
  group_by(plot_group) %>%
  summarise(n_stems = n(),
    var_height = var(height, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    sd_height = sd(height, na.rm = TRUE),
    max_height = max(height, na.rm = TRUE),
    var_dbh = var(diam, na.rm = TRUE),
    mean_dbh = mean(diam, na.rm = TRUE),
    sd_dbh = sd(diam, na.rm = TRUE),
    max_dbh = max(diam, na.rm = TRUE)) %>%
  mutate(cov_height = sd_height / mean_height * 100,
    cov_dbh = sd_dbh / mean_dbh * 100)

# Calculate shannon index of diameter classes
s_fil$diam_bin <- cut(s_fil$diam, breaks = seq(from = 5, to = 185, by = 5), 
  labels = dbh_labels, include.lowest = TRUE)

diam_fil_even <- s_fil %>%
  group_by(plot_group, diam_bin, .drop = FALSE) %>%
  tally() %>%
  spread(diam_bin, n, fill = 0) %>%
  ungroup() %>%
  data.frame() 

row.names(diam_fil_even) <- diam_fil_even$plot_group
diam_fil_even_clean <- dplyr::select(diam_fil_even, -plot_group)

diam_even <- data.frame(diam_shannon = diversity(diam_fil_even_clean), 
  diam_bin_n = rowSums(diam_fil_even_clean > 0))
diam_even$plot_group <- row.names(diam_even)
diam_even$diam_even <- diam_even$diam_shannon / diam_even$diam_bin_n

# Mean of 95th percentile of tree biomass of each plot
s_fil_bchave_95 <- s_fil %>%
  group_by(plot_group) %>%
  filter(Bchave > quantile(Bchave, 0.95)) %>%
  summarise(bchave_mean_95 = sum(Bchave))

quantile_lo <- seq(from = 0, to = 0.95, by = 0.05)
quantile_hi <- seq(from = 0.05, to = 1, by = 0.05)

bchave_quantile_list <- list()
for(i in 1:length(quantile_lo)){
  varname <- paste0("bchave_mean_95_", quantile_lo[[i]], "-", quantile_hi[[i]])
  varname_n <- paste0("n_", quantile_lo[[i]], "-", quantile_hi[[i]])
  
  bchave_quantile_list[[i]] <- s_fil %>%
    group_by(plot_group) %>%
    filter(Bchave < quantile(Bchave, quantile_hi[[i]]), 
      Bchave > quantile(Bchave, quantile_lo[[i]])) %>%
    summarise(!!varname := sum(Bchave),
      !!varname_n := n())
}

bchave_quantile_df <- purrr::reduce(bchave_quantile_list, full_join, by = "plot_group")

# Clean dataframe, add stem summary data ----
plot_data_agg <- left_join(plot_data_agg, s_fil_summ, by = c("plot_group" = "plot_group")) %>%
  left_join(., diam_even, by =  c("plot_group" = "plot_group")) %>%
  left_join(., s_fil_bchave_95, by =  c("plot_group" = "plot_group")) %>%
  mutate(stems_ha = n_stems / area_of_plot) %>%
  filter(area_of_plot >= 0.1,
  stems_ha >= 10) %>%
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
    -goats_graze_here)

# Estimate rarefied species diversity ----

# Create matrix for estimating rarefied sp. rich.
## Aggregate by plot_group
ab_mat <- s_fil %>% 
  filter(plot_group %in% plot_data_agg$plot_group) %>%
  group_by(plot_group, gen_sp, .drop = FALSE) %>%
  tally() %>%
  spread(gen_sp, n, fill = 0) %>%
  ungroup() %>%
  data.frame() 

# Make tidy
row.names(ab_mat) <- ab_mat$plot_group
ab_mat_clean <- dplyr::select(ab_mat, -plot_group)

# Hill number estimation of species richness and shannon index
chao_rich_list <- apply(ab_mat_clean, 1, ChaoRichness)

chao_rich_df <- do.call("rbind", chao_rich_list)

chao_rich_df$plot_group <- row.names(chao_rich_df)

names(chao_rich_df) <- c("sp_r", "sp_rich_raref", "sp_rich_raref_se", "sp_rich_lower_ci_95", "sp_rich_upper_ci_95", "plot_group")

chao_shannon_list <- apply(ab_mat_clean, 1, ChaoShannon, transform = TRUE)

chao_shannon_df <- do.call("rbind", chao_shannon_list)

chao_shannon_df$plot_group <- row.names(chao_shannon_df)

names(chao_shannon_df) <- c("shannon_obs", "shannon_exp", "shannon_se", "shannon_lower_ci_95", "shannon_upper_ci_95", "plot_group")

chao_df <- left_join(chao_rich_df, chao_shannon_df, by = c("plot_group" = "plot_group"))

# Join estimated species richness data to main data
plot_data_agg <- left_join(plot_data_agg, chao_df, by = c("plot_group", "plot_group"))

# Estimate Species abundance evenness from Shannon
plot_data_agg$shannon_equit <- plot_data_agg$shannon_exp / plot_data_agg$sp_rich_raref

# Remove plots with zero Shannon equitability and shannon index
plot_data_agg <- plot_data_agg %>%
  filter(shannon_equit > 0,
    shannon_exp > 0) %>%
# Exclude big 10 Ha plot
  filter(plotcode != "DKS001")# %>%
# Exclude plots where min diam. is not less than ten cm
 # filter(plot_group %in% s_diam_summ$plot_group)

# Ensure that variables all have the same sign
plot_data_agg$precip_seasonality_rev <- -1 * plot_data_agg$precip_seasonality + 1000
plot_data_agg$temp_seasonality_rev <- -1 * plot_data_agg$temp_seasonality + 1000
plot_data_agg$sand_per_rev <- -1 * plot_data_agg$sand_per + 1000
plot_data_agg$mean_temp_rev <- -1 * plot_data_agg$mean_temp + 1000

# Exclude clust5-5/clust4-4
#plot_data_agg <- plot_data_agg %>%
#  filter(clust4 != 4)

# Randomly take 50 plots from C4-2 and add to C4-3
c2_c3_vec <- plot_data_agg %>%
  filter(clust4 == 2) %>%
  sample_n(50) %>%
  pull(plot_group)

plot_data_agg <- plot_data_agg %>%
  mutate(clust4 = case_when(
    plot_group %in% c2_c3_vec ~ 3,
    TRUE ~ as.double(clust4)))


if(full_best == "full"){

# Save final DBH lookup
plotcode_plot_group_lookup <- plot_data_agg %>%
  dplyr::select(plotcode_vec, plot_group) %>%
  unnest(plotcode_vec)
  
write.csv(plotcode_plot_group_lookup, paste0("data/plotcode_plot_group_lookup", ext, ".csv"), row.names = FALSE)

plot_data_agg <- plot_data_agg %>% dplyr::select(-plotcode_vec)

# Write to CSV
write.csv(plot_data_agg, "data/plot_data_fil_agg.csv", row.names = FALSE)
  
  }else{
  
# Optionally only pick the "best" plots
##' The 200-300 plots that are the most foresty
##' measured by stems ha and/or biomass
plot_data_agg_best <- subset(plot_data_agg, 
  subset=(plot_data_agg$stems_ha >= quantile(plot_data_agg$stems_ha, 0.80)))

write.csv(plot_data_agg_best, "data/plot_data_fil_agg_best.csv", row.names = FALSE)
  }

# Save DBH quantiles 
bchave_quantile_df <- left_join(plot_data_agg, bchave_quantile_df, by = "plot_group") %>%
  select(plot_group, sp_rich, sp_rich_raref, starts_with("bchave_mean_95_0"))

write.csv(bchave_quantile_df, "data/bchave_quantile.csv", row.names = FALSE)

