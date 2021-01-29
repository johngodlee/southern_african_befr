# Creattng a dataset 
# John Godlee (johngodlee@gmail.com)
# 2018-12-10
# 2019-12-11
# 2020-06-22
# 2020-08-19
# 2021-01-29

# Packages
library(dplyr)
library(tidyr)  
library(iNEXT)  
library(vegan) 
library(ggplot2)
library(seosawr)
library(raster)
library(sf)

# Import plot and stems data ----

# SEOSAW v1 plot summary data, for vegetation clusters
load("data/seosaw_plot_summary5Apr2019.Rdata")

# SEOSAW v2 plot summary data
plots <- read.csv("~/git_proj/seosaw_data/data_out/v2.7/plots_v2.7.csv")

# SEOSAW v2 stem data
stems <- read.csv("~/git_proj/seosaw_data/data_out/v2.7/stems_latest_v2.7.csv")

# SEOSAW v2.10 spatial data, for nitrogen, sand, fire, etc.
spatial <- read.csv("~/git_proj/seosaw_data/data_out/v2.10/plots_spatial_v2.10.csv")

# Datasets used in analysis
dataset_codes <- c("ZIS", "SSM", "MCL", "ZPF", "ZNF", "MNR", "MGR", "MAR",
  "ZKS", "TKW", "MLC", "ZCC", "VAS", "ABG", "MCF", "MNF", "SHD", "DKS")

# Clean plots data
plots_clean <- plots %>%
  mutate(ntfp_harvesting = case_when(
      grepl("DKS", plot_id) ~ FALSE,
      TRUE ~ ntfp_harvesting),
    farmed_30_years = case_when(
      grepl("DKS", plot_id) ~ FALSE,
      TRUE ~ farmed_30_years)) %>%  # Fix incorrect metadata
  filter_at(.vars = c("charcoal_harvesting", "timber_harvesting", 
    "high_graded_100_years", "manipulation_experiment", "fuel_wood_harvesting", 
    "other_woody_product_harvesting", "ntfp_harvesting", "farmed_30_years",
    "fire_exclusion", "fire_treatment", "cattle_grazing", "goat_grazing"),
    all_vars(. %in% c(FALSE, NA))) %>%  # Remove human-altered plots
    filter(grepl(paste(dataset_codes, 
        collapse = "|"), plot_id)) %>%  # Only datasets I want
    left_join(., spatial, by = "plot_id") %>%  # Add spatial data
    dplyr::select(
      plot_cluster, 
      plot_id,
      longitude_of_centre,
      latitude_of_centre,
      plot_area, 
      cec = soil_cation_ex_cap, 
      sand = soil_sand, 
      soil_c = soil_org_c, 
      nitrogen = soil_nitrogen,
      temp = bio1.y, 
      temp_seas = bio4.y, 
      temp_stress = bio2.y,
      precip = bio12.y, 
      precip_seas = bio15.y, 
      fire = fire_count,
      herbivory = herbiv_total)  # Columns I want

# Create Zambia ILUAii only dataset
plots_zam <- plots_clean %>%
  filter(grepl("ZIS", plot_id))

# Subset to plot clusters which contain 4 plots
zam_full_clust <- plots_zam %>%
  group_by(plot_cluster) %>%
  tally() %>%
  filter(n == 4) %>% 
  pull(plot_cluster)

plots_zam <- plots_zam %>%
  filter(plot_cluster %in% zam_full_clust)

# How many plots and clusters in Zambia data?
nzam <- nrow(plots_zam)
nzamcluster <- nzam / 4

# Create non-Zambia only dataset
plots_nozam <- plots_clean %>%
  filter(!grepl("ZIS", plot_id)) %>%
  mutate(plot_cluster = plot_id)

# Aggregate values for Zambian plots by plot cluster
plots_zam_agg <- plots_zam %>% 
  group_by(plot_cluster) %>%  
  summarise(plot_id = paste0(plot_id, collapse = ","),
    longitude_of_centre = mean(longitude_of_centre, na.rm = TRUE),
    latitude_of_centre = mean(latitude_of_centre, na.rm = TRUE),
    plot_area = sum(plot_area, na.rm = TRUE),
    cec = mean(cec, na.rm = TRUE),
    sand = mean(sand, na.rm = TRUE),
    soil_c = mean(soil_c, na.rm = TRUE),
    precip = mean(precip, na.rm = TRUE),
    precip_seas = mean(precip_seas, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    temp_seas = mean(temp_seas, na.rm = TRUE),
    temp_stress = mean(temp_stress, na.rm = TRUE),
    fire = mean(fire, na.rm = TRUE),
    herbivory = mean(herbivory, na.rm = TRUE))

# Combine Zambian aggregated data with non-aggregated other data
plots_agg <- bind_rows(plots_zam_agg, plots_nozam)

# Add plot_cluster vector to dataframe
plots_agg$plot_id_vec <- strsplit(as.character(plots_agg$plot_id), 
  split=",")

# Create plot_id vs plot_cluster lookup table
plot_id_plot_cluster_lookup <- plots_agg %>%
  dplyr::select(plot_id = plot_id_vec, plot_cluster) %>%
  unnest(plot_id)

# Filter stems to >10 cm DBH, alive, and trees with species 
stems_fil <- left_join(stems, plot_id_plot_cluster_lookup, by = "plot_id") %>%
  mutate(
    species_name_clean = case_when(
      is.na(species_name_clean) ~ species_orig_binom,
      TRUE ~ species_name_clean)) %>%
  filter(
    !is.na(plot_cluster),
    !is.na(species_name_clean),
    !grepl("indet", species_name_clean, ignore.case = TRUE),
    diam >= 10,
    alive %in% c("A", NA))

# Calculate plot level statistics
stems_summ <- stems_fil %>%
  group_by(plot_cluster) %>%
  summarise(
    agb = sum(agb, na.rm = TRUE),
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

# Join to plots table
plots_stems <- left_join(stems_summ, plots_agg, by = "plot_cluster")

# Calculate AGB per ha
plots_stems$agb_ha <- plots_stems$agb / plots_stems$plot_area

# Create abundance matrix by tree_id
ab_mat <- abMatGen(stems_fil, plot_id = "plot_cluster", tree_id = "tree_id",
  species_name = "species_name_clean", fpc = "fpc")

# Hill number estimation of species richness and shannon index
ab_mat_t <- as.data.frame(t(ab_mat))

chao_list <- iNEXT(ab_mat_t, q = 0)

chao_rich_df <- chao_list$AsyEst %>%
  filter(Diversity == "Species richness") %>%
  dplyr::select(plot_cluster = Site, 
    n_species_raref = Estimator, n_species_raref_se = s.e., 
    n_species_lower_ci_95 = LCL, n_species_upper_ci_95 = UCL)

chao_shannon_df <- chao_list$AsyEst %>%
  filter(Diversity == "Shannon diversity") %>%
  dplyr::select(plot_cluster = Site, shannon_exp = Estimator, 
    shannon_se = s.e., shannon_lower_ci_95 = LCL, shannon_upper_ci_95 = UCL)

chao_df <- left_join(chao_rich_df, chao_shannon_df, by = "plot_cluster")

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

# ab_mat tally per plot
ab_mat_tally <- rowSums(ab_mat)
ab_mat_tally_df <- data.frame(n_trees_gt10 = ab_mat_tally, 
  plot_cluster = names(ab_mat_tally))

# Add tree diversity data to plot level data 
plots_div <- left_join(plots_stems, chao_df, by = "plot_cluster") %>%
  left_join(., ab_mat_tally_df, by = "plot_cluster") %>%
  mutate(n_trees_gt10_ha = n_trees_gt10 / plot_area)

# Remove plots with less than 50 trees per ha, less than 0.1 ha plot area 
plots_div_fil <- plots_div %>%
  filter(n_trees_gt10_ha >= 50,
    plot_area >= 0.1)

# Make clean plot cluster dataframe
clust <- ssaw8$cluster %>% 
  filter(grepl(paste(dataset_codes, collapse = "|"), plotcode)) %>%
  mutate(plot_id = gsub("-(0)?(0)?", "_", plotcode)) %>%
  dplyr::select(plot_id, starts_with("clust")) %>%
  mutate(plot_id = case_when(
      plot_id == "DKS001" ~ "DKS_1",
      plot_id == "DKS002" ~ "DKS_2",
      plot_id == "DKS003" ~ "DKS_3",
      TRUE ~ plot_id))

# Add Bicuar plots which didn't have clusters assigned
bicuar_clust <- data.frame(plot_id = paste0("ABG_", seq(5, 15)), 
  clust7 = NA, clust5 = NA, clust4 = 1)

clust <- rbind(clust, bicuar_clust)

# Add cluster data to plots
plots_clust <- left_join(plots_div_fil, clust, 
  by = c("plot_cluster" = "plot_id")) %>%
  filter(!is.na(clust4))

# Write plot cluster ID lookup table
plotcode_plot_cluster_lookup <- plots_clust %>%
  dplyr::select(plot_id_vec, plot_cluster) %>%
  unnest(plot_id_vec)

saveRDS(plotcode_plot_cluster_lookup, "data/plotcode_plot_cluster_lookup.rds")

# Write plot-level data
plots_clust_clean <- plots_clust %>% 
  dplyr::select(-plot_id_vec, -plot_id)

saveRDS(plots_clust_clean, "data/plot_data_fil_agg.rds")

# Write tree abundance matrix
ab_mat_clean <- ab_mat %>%
  rownames_to_column("plot_cluster") %>%
  filter(plot_cluster %in% plots_clust_clean$plot_cluster) %>%
  column_to_rownames("plot_cluster") %>%
  dplyr::select(where(~sum(.x) != 0))

saveRDS(ab_mat_clean, "data/trees_ab_mat.rds")

# Write stems data
stems_fil_clean <- stems_fil %>%
  filter(plot_cluster %in% plots_clust_clean$plot_cluster) 

saveRDS(stems_fil_clean, "data/stems.rds")

# Write statistics for manuscript
fileConn <- file(paste0("include/zam.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\nzam}{", nzam, "}"),
    paste0("\\newcommand{\\nzamcluster}{", nzamcluster, "}")
    ),
  fileConn)
close(fileConn)
