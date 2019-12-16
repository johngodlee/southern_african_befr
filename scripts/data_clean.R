# Creating a dataset for use in Chapter 1:
# Regional analysis of the factors affecting woody AGB and productivity
# John Godlee (johngodlee@gmail.com)
# 2018_12_10
# 2019_12_11

# Preamble ----

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)  #
library(tidyr)  #
library(iNEXT)  #
library(vegan)  # 
library(ggplot2) # 

# Import data ----

# Base SEOSAW plot summary dataset
load("data/seosaw_plot_summary5Apr2019.Rdata")

# Per stem data 
load("data/clean_input_data.Rdata")

# Soil organic carbon
ocdens <- readRDS("data/seosaw_ocdens.rds")

# WorldClim temp.
temp <- readRDS("data/plot_temp.rds")

# WorldClim precip.
precip <- readRDS("data/plot_precip.rds")

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
  left_join(., ocdens, by = c("plotcode" = "plotcode")) %>%
  left_join(., temp, by = c("plotcode" = "plotcode")) %>%
  left_join(., precip, by = c("plotcode" = "plotcode")) %>%
  dplyr::select(plotcode,
    long = longitude_of_centre.x.x,
    lat = latitude_of_centre.x.x,
    country = country.x,
    area_plot = area_of_plot.x, 
    shape_of_plot, 
    manipulation_experiment, 
    timber_harvesting_occurring, 
    was_the_plot_high_graded_in_last_100_years, 
    fuel_wood_harvesting, charcoal_harvesting, 
    other_woody_product_harvesting, ntfp_harvesting,
    used_for_farming_in_last_30_years, 
    protected_from_fire, elephant_exclosure, 
    cattle_graze_here, goats_graze_here,
    cec = CECSOL_M_sl1_1km_ll,
    sand_per = SNDPPT_M_sl1_1km_ll,
    org_c_per = ORCDRC_M_sl1_1km_ll,
    total_precip = p_plot,
    precip_seasonality = p_cv_plot,
    ocdens = ocdens,
    mean_temp = t_plot,
    isothermality = bio3,
    temp_seasonality = t_cv_plot,
    pi, 
    plot_id,
    clust4) %>%
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
    !is.na(clust4))

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
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    country = first(na.omit(country)),
    area_plot = sum(area_plot, na.rm = TRUE),
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
    cec = mean(cec, na.rm = TRUE),
    sand_per = mean(sand_per, na.rm = TRUE),
    org_c_per = mean(org_c_per, na.rm = TRUE),
    total_precip = mean(total_precip, na.rm = TRUE),
    precip_seasonality = mean(precip_seasonality, na.rm = TRUE),
    ocdens = mean(ocdens, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    temp_seasonality = mean(temp_seasonality, na.rm = TRUE),
    isothermality = mean(isothermality, na.rm = TRUE),
    pi = first(na.omit(pi)),
    plot_id = first(na.omit(plot_id)),
    clust4 = first(na.omit(clust4))) 

# Combine Zambian aggregated data with non-aggregated other data ----
plot_data_agg <- bind_rows(plot_data_zam_agg, plot_data_nozam) %>%
  mutate(plot_group = if_else(!is.na(plot_group), plot_group, plotcode))

# Add plot_group identifier to stem level data ----
plot_data_agg$plotcode_vec <- strsplit(as.character(plot_data_agg$plotcode), 
  split=",")

plotcode_plot_group_lookup <- plot_data_agg %>%
  dplyr::select(plotcode_vec, plot_group) %>%
  unnest(plotcode_vec)

s <- left_join(s, plotcode_plot_group_lookup, by = c("plotcode" = "plotcode_vec"))

# Calculate plot level values from stems ----
# Filter to big trees, alive trees, and trees with identity
s_fil <- s %>%
  filter(
    !is.na(plot_group),
    !is.na(gen_sp),
    diam >= 10,
    alive %in% c("A", NA))

s_fil_summ <- s_fil %>%
  group_by(plot_group) %>%
  summarise(
    n_stems = n(),
    n_species = n_distinct(gen_sp),
    agb = sum(Bchave, na.rm = TRUE),
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

# Remove tiny tree-less plots ----
plot_data_agg <- left_join(plot_data_agg, s_fil_summ, 
  by = c("plot_group" = "plot_group")) %>%
  mutate(stems_ha = n_stems / area_plot,
    agb_ha = agb / area_plot) %>%
  filter(area_plot >= 0.1,
    stems_ha >= 10) %>%
  filter(plot_group != "DKS001")

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

# Save for later use
saveRDS(ab_mat_clean, "data/stems_ab_mat.rds")

# Hill number estimation of species richness and shannon index
ab_mat_clean_t <- as.data.frame(t(ab_mat_clean))

chao_list <- iNEXT(ab_mat_clean_t, q = 0)

chao_rich_df <- chao_list$AsyEst %>%
  filter(Diversity == "Species richness") %>%
  dplyr::select(plot_group = Site, n_species_raref = Estimator, 
    n_species_raref_se = s.e., n_species_lower_ci_95 = LCL, n_species_upper_ci_95 = UCL)

chao_shannon_df <- chao_list$AsyEst %>%
  filter(Diversity == "Shannon diversity") %>%
  dplyr::select(plot_group = Site, shannon_exp = Estimator, 
    shannon_se = s.e., shannon_lower_ci_95 = LCL, shannon_upper_ci_95 = UCL)

chao_df <- left_join(chao_rich_df, chao_shannon_df, by = c("plot_group" = "plot_group"))

# Estimate Species abundance evenness from Shannon
chao_df$shannon_equit <- chao_df$shannon_exp / chao_df$n_species_raref

# Plot extrapolated estimates 
inext_output_bind <- bind_rows(chao_list$iNextEst, .id = "id")
inext_output_bind_obs <- inext_output_bind %>% filter(method == "observed")
inext_output_bind_interp <- inext_output_bind %>% filter(method == "interpolated")
inext_output_bind_extrap <- inext_output_bind %>% filter(method == "extrapolated")
inext_output_bind_extrap_double <- inext_output_bind %>% group_by(id) %>%
  summarise(m_max = max(m), qd_max = max(qD))

pdf("img/chao_richness_extrap.pdf", width = 12, height = 6)
ggplot() +
  geom_line(data = inext_output_bind_interp,
    aes(x = m, y = qD, group = id)) +
  geom_line(data = inext_output_bind_extrap,
    aes(x = m, y = qD, group = id),
    linetype = 2) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Bootstrap sample size", y = "Extrapolated species richness")
dev.off()

# Clean dataframe, add stem summary data ----
plot_data_agg_clean <- left_join(plot_data_agg, chao_df, by = c("plot_group" = "plot_group")) %>%
  dplyr::select(plot_group, plotcode, plotcode_vec, plot_id, country, pi, area_plot, clust4,
    long, lat, 
    cec, sand_per, org_c_per, ocdens,
    total_precip, precip_seasonality, mean_temp, temp_seasonality, isothermality,
    n_stems, n_species, n_species_raref, agb_ha, stems_ha,
    shannon_exp, shannon_equit,
    var_height, mean_height, sd_height, max_height, cov_height,
    var_dbh, mean_dbh, sd_dbh, max_dbh, cov_dbh)

# Ensure that variables all have the same direciton for SEM
plot_data_agg_clean$precip_seasonality_rev <- -1 * plot_data_agg_clean$precip_seasonality + 1000
plot_data_agg_clean$temp_seasonality_rev <- -1 * plot_data_agg_clean$temp_seasonality + 1000
plot_data_agg_clean$sand_per_rev <- -1 * plot_data_agg_clean$sand_per + 1000
plot_data_agg_clean$mean_temp_rev <- -1 * plot_data_agg_clean$mean_temp + 1000
plot_data_agg_clean$shannon_equit <- -1 * plot_data_agg_clean$shannon_equit + 1000

# Write data ----
# Plot group - plotcode lookup 
plotcode_plot_group_lookup <- plot_data_agg_clean %>%
  dplyr::select(plotcode_vec, plot_group) %>%
  unnest(plotcode_vec)
saveRDS(plotcode_plot_group_lookup, "data/plotcode_plot_group_lookup.rds")

# Plot data
plot_data_agg_clean <- plot_data_agg_clean %>% dplyr::select(-plotcode_vec)
saveRDS(plot_data_agg_clean, "data/plot_data_fil_agg.rds")

