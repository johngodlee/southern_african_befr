# Creating a dataset for use in Chapter 1:
# Regional analysis of the factors affecting woody AGB and productivity
# John Godlee (johngodlee@gmail.com)
# 2018-12-10
# 2019-12-11
# 2020-06-22
# 2020-08-19

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
plots <- read.csv("~/git_proj/seosaw_data/data_out/plots_v2.7.csv")

# SEOSAW v2 stem data
stems <- read.csv("~/git_proj/seosaw_data/data_out/stems_latest_v2.7.csv")

# Fire layer 
fire_count <- raster("/Volumes/john/AFcount_2001_2018.tif")
 
# Datasets used in analysis
dataset_codes <- c("ZIS", "SSM", "MCL", "ZPF", "ZNF", "MNR", "MGR", "MAR",
  "ZKS", "TKW", "MLC", "ZCC", "VAS", "ABG", "MCF", "MNF", "SHD", "DKS")

# Remove plots with human-altered conditions ----

# Fix some incorrect metadata
plots_fix <- plots %>% 
  mutate(ntfp_harvesting = case_when(
      grepl("DKS", plot_id) ~ FALSE,
      TRUE ~ ntfp_harvesting),
    farmed_30_years = case_when(
      grepl("DKS", plot_id) ~ FALSE,
      TRUE ~ farmed_30_years))

plots_clean <- plots_fix %>%
  filter_at(.vars = c("charcoal_harvesting", "timber_harvesting", 
    "high_graded_100_years", "manipulation_experiment", "fuel_wood_harvesting", 
    "other_woody_product_harvesting", "ntfp_harvesting", "farmed_30_years",
    "fire_exclusion", "fire_treatment", "cattle_grazing", "goat_grazing"),
    all_vars(. %in% c(FALSE, NA))) %>%
    filter(grepl(paste(dataset_codes, 
        collapse = "|"), plot_id)) %>%
    dplyr::select(
      plot_cluster, 
      plot_id,
      prinv,
      longitude_of_centre,
      latitude_of_centre, 
      plot_area, 
      cec = CECSOL, 
      sand = SNDPPT, 
      soil_c = ORCDRC, 
      temp = bio1, 
      temp_seas = bio4, 
      temp_stress = bio2,
      precip = bio12, 
      precip_seas = bio15, 
      fire = firecount_2001_2018,
      herbivory = total_herbivory)

# Aggregate Zambian Forestry Commission plots ----

# Split into Zambia and non-Zambia datasets
plots_zam <- plots_clean %>%
  filter(prinv == "Siampale A.")

zamfullcluster <- plots_zam %>%
  group_by(plot_cluster) %>%
  tally() %>%
  filter(n == 4) 

plots_zam <- plots_zam %>%
  filter(plot_cluster %in% zamfullcluster$plot_cluster)

nzam <- nrow(plots_zam)
nzamcluster <- nzam / 4

plots_nozam <- plots_clean %>%
  filter(prinv != "Siampale A.") %>%
  mutate(plot_cluster = plot_id)

# Aggregate values for Zambian plots and randomly sample rows
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
    herbivory = mean(herbivory, na.rm = TRUE),
    prinv = first(na.omit(prinv)))

# Combine Zambian aggregated data with non-aggregated other data
plots_agg <- bind_rows(plots_zam_agg, plots_nozam)

# Calculate plot level values from stems ----

# Add plot_cluster identifier to stem level data
plots_agg$plot_id_vec <- strsplit(as.character(plots_agg$plot_id), 
  split=",")

plot_id_plot_cluster_lookup <- plots_agg %>%
  dplyr::select(plot_id_vec, plot_cluster) %>%
  unnest(plot_id_vec)

# Filter stems to big trees, alive trees, and trees with species identity
stems_fil <- left_join(stems, plot_id_plot_cluster_lookup, 
  by = c("plot_id" = "plot_id_vec")) %>%
  mutate(species_name_clean = case_when(
      is.na(species_name_clean) ~ species_orig_binom,
      TRUE ~ species_name_clean)) %>%
  filter(
    !is.na(plot_cluster),
    !is.na(species_name_clean),
    !grepl("indet", species_name_clean, ignore.case = TRUE),
    diam >= 10,
    alive %in% c("A", NA))

# Calculate plot level statistics
stems_fil_summ <- stems_fil %>%
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
plots_stems <- left_join(stems_fil_summ, plots_agg, by = "plot_cluster")

# Calculate AGB per ha
plots_stems$agb_ha <- plots_stems$agb / plots_stems$plot_area

# Estimate rarefied species diversity statistics ----
# Create abundance matrix by tree_id
ab_mat <- abMatGen(stems_fil, plot_id = "plot_cluster", tree_id = "tree_id",
  species_name = "species_name_clean", fpc = "fpc")

# Hill number estimation of species richness and shannon index
ab_mat_t <- as.data.frame(t(ab_mat))

chao_list <- iNEXT(ab_mat_t, q = 0)

chao_rich_df <- chao_list$AsyEst %>%
  filter(Diversity == "Species richness") %>%
  dplyr::select(plot_cluster = Site, n_species_raref = Estimator, 
    n_species_raref_se = s.e., n_species_lower_ci_95 = LCL, n_species_upper_ci_95 = UCL)

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

# Add tree diversity data to plot level data ----
plots_div <- left_join(plots_stems, chao_df, by = "plot_cluster") %>%
  left_join(., as.data.frame(rowSums(ab_mat)) %>%
  mutate(plot_cluster = row.names(.)) %>%
  rename(n_trees_gt10 = `rowSums(ab_mat)`), by = "plot_cluster") %>%
  mutate(n_trees_gt10_ha = n_trees_gt10 / plot_area)

# Remove plots with less than 50 trees per ha, less than 0.1 ha plot area ----
plots_div_clean <- plots_div %>%
  filter(n_trees_gt10_ha >= 50,
    plot_area >= 0.1)

# Add plot clusters from old data ----
clust <- ssaw8$cluster %>% 
  filter(grepl(paste(dataset_codes, 
    collapse = "|"), plotcode)) %>%
  mutate(plot_id = gsub("-(0)?(0)?", "_", plotcode)) %>%
  dplyr::select(plot_id, starts_with("clust")) %>%
  mutate(plot_id = case_when(
      plot_id == "DKS001" ~ "DKS_1",
      plot_id == "DKS002" ~ "DKS_2",
      plot_id == "DKS003" ~ "DKS_3",
      TRUE ~ plot_id))

# Add Bicuar plots which didn't have clusters assigned
clustadd <- data.frame(plot_id = paste0("ABG_", seq(5, 15)), 
  clust7 = NA, clust5 = NA, clust4 = 1)

clust <- rbind(clust, clustadd)

plots_clust <- left_join(plots_div_clean, clust, 
  by = c("plot_cluster" = "plot_id")) %>%
  filter(!is.na(clust4))

# Extract soil Nitrogren from soil Grids ----

#write.csv(plots_clust[,c("longitude_of_centre", "latitude_of_centre")], 
#  "../data/plots_coord.csv", row.names = FALSE)

##' Run external script `scripts/nitrogen_get.sh`

nitrogen <- readLines("data/nitrogen.txt")
plots_clust$nitrogen <- as.numeric(nitrogen)

# Write data ----
# Plot group - plotcode lookup 
plotcode_plot_cluster_lookup <- plots_clust %>%
  dplyr::select(plot_id_vec, plot_cluster) %>%
  unnest(plot_id_vec)
saveRDS(plotcode_plot_cluster_lookup, "data/plotcode_plot_cluster_lookup.rds")

# Plot data
plots_clust_clean <- plots_clust %>% 
  dplyr::select(-plot_id_vec, -plot_id, -prinv)
saveRDS(plots_clust_clean, "data/plot_data_fil_agg.rds")

# Tree abundance matrix
ab_mat$plot_cluster <- row.names(ab_mat)
ab_mat_clean <- ab_mat %>%
  filter(plot_cluster %in% plots_clust_clean$plot_cluster) %>%
  dplyr::select(-plot_cluster)
ab_mat_clean_clean <- ab_mat_clean[,!colSums(ab_mat_clean) == 0]

saveRDS(ab_mat_clean_clean, "data/stems_ab_mat.rds")

fileConn <- file(paste0("include/zam.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\nzam}{", nzam, "}"),
    paste0("\\newcommand{\\nzamcluster}{", nzamcluster, "}")
    ),
  fileConn)
close(fileConn)
