# Extract TRY database values
# John Godlee (johngodlee@gmail.com)
# 2021-01-25

# Packages
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(BIOMASS)
library(purrr)
library(broom)

source("scripts/clust_defin.R")

# Import data
try_dat <- fread("data/try/13399.txt", 
  header = TRUE, sep = "\t", dec = ".", quote = "", data.table = FALSE)

 Species from SEOSAW
plots <- readRDS("data/plot_data_fil_agg_norm_std.rds")
stems <- readRDS("data/stems.rds")

# How many species are we searching for?
length(unique(stems$species_name_clean))

# Find wood density values per species from Zanne et al. 2009
wd_sp <- BIOMASS::wdData %>%
  mutate(species = paste(genus, species)) %>%
  filter(
    species %in% unique(stems$species_name_clean),
    grepl("africa", region, ignore.case = TRUE)) %>%
  group_by(species) %>%
  summarise(val_std = mean(wd, na.rm = TRUE)) %>%
  mutate(trait_id = "wd")

# Clean TRY data
try_clean <- try_dat %>%
  dplyr::select(
    dataset = Dataset,
    obs_id = ObservationID,
    species_id = AccSpeciesID,
    species = AccSpeciesName,
    trait_id = TraitID,
    trait = TraitName,
    key_id = DataID,
    key = DataName,
    val = OrigValueStr,
    unit = OrigUnitStr,
    val_std = StdValue,
    unit_std = UnitName,
    error_risk = ErrorRisk)

# Trait ID match
trait_id_lookup <- try_clean %>%
  dplyr::select(trait_id, trait) %>%
  filter(
    !is.na(trait_id),
    trait_id %in% c(14, 15, 3115, 3116, 3117)) %>%
  distinct() %>%
  mutate(
    trait = case_when(
      trait_id %in% c(3115, 3116, 3117) ~ gsub(":.*", "", trait),
      TRUE ~ trait),
    trait_short = case_when(
      trait_id == 14 ~ "Leaf N / dry mass",
      trait_id == 15 ~ "Leaf P / dry mass",
      trait_id %in% c(3115, 3116, 3117) ~ "SLA",
      TRUE ~ as.character(trait_id)),
    trait_col = case_when(
      trait_id == 14 ~ "leaf_n",
      trait_id == 15 ~ "leaf_p",
      trait_id %in% c(3115, 3116, 3117) ~ "sla",
      TRUE ~ as.character(trait_id))
  )

# Split try data by observation
try_split <- split(try_clean, try_clean$obs_id)

# Create clean dataframe of observations
try_species <- as.data.frame(do.call(rbind, lapply(try_split, function(x) {
  traits <- x[x$trait_id %in% trait_id_lookup$trait_id,
    c("species", "trait_id", "val_std")]

  traits$trait_id <- trait_id_lookup$trait_col[
  match(traits$trait_id, trait_id_lookup$trait_id)]

  longitude <- as.numeric(x[x$key_id == 60, "val_std"])
  if (length(longitude) == 0) {
    traits$longitude <- NA_real_
  } else {
    traits$longitude <- longitude
  }

  latitude <- as.numeric(x[x$key_id == 59, "val_std"])
  if (length(latitude) == 0) {
    traits$latitude <- NA_real_
  } else {
    traits$latitude <- latitude
  }
  return(traits)
    })))

# Add wood density to traits dataframe
traits <- bind_rows(wd_sp, try_species)

# Create summary of traits by species
traits_species_summ <- traits %>%
  group_by(species, trait_id) %>%
  summarise(val_std = mean(val_std, na.rm = TRUE),
    n_meas = n()) %>%
  filter(!is.na(trait_id), !is.nan(val_std)) %>%
  pivot_wider(id_cols = species, 
            names_from = trait_id, 
            values_from = c("val_std", "n_meas")) %>%
  dplyr::select(-n_meas_wd)

# Add traits to stems dataframe
stems_species_traits <- left_join(stems, 
  traits_species_summ %>% 
    filter(species %in% stems$species_name_clean) %>%
    dplyr::select(species, starts_with("val_std")),
  by = c("species_name_clean" = "species"))

# Split stems data by plot
traits_species_plot_split <- split(stems_species_traits, stems_species_traits$plot_cluster)

# For each plot, proportion of trees and ba represented for each trait
traits_species_prop <- do.call(rbind, lapply(traits_species_plot_split, function(x) {
    n_trees <- length(unique(x$tree_id))
    ba <- sum(x$ba, na.rm = TRUE)
  data.frame(
    plot_cluster = unique(x$plot_cluster),
    n_trees,
    ba,
    prop_ind_leaf_n = length(unique(x[!is.na(x$val_std_leaf_n), "tree_id"])) / n_trees,
    prop_ind_leaf_p = length(unique(x[!is.na(x$val_std_leaf_p), "tree_id"])) / n_trees,
    prop_ind_sla = length(unique(x[!is.na(x$val_std_sla), "tree_id"])) / n_trees,
    prop_ind_wd = length(unique(x[!is.na(x$val_std_wd), "tree_id"])) / n_trees,
    prop_ba_leaf_n = sum(x[!is.na(x$val_std_leaf_n), "ba"], na.rm = TRUE) / ba ,
    prop_ba_leaf_p = sum(x[!is.na(x$val_std_leaf_p), "ba"], na.rm = TRUE) / ba,
    prop_ba_sla = sum(x[!is.na(x$val_std_sla), "ba"], na.rm = TRUE) / ba,
    prop_ba_wd = sum(x[!is.na(x$val_std_wd), "ba"], na.rm = TRUE) / ba
  )
}))

# Make a clean dataframe
traits_species_prop_clean <- traits_species_prop %>%
  gather(trait, prop, -plot_cluster, -n_trees, -ba) %>%
  mutate(
    method = case_when(
      grepl("ind", trait) ~ "N trees",
      grepl("ba", trait) ~ "Basal area",
      TRUE ~ trait),
    trait = case_when(
      grepl("leaf_n", trait) ~ "Leaf N",
      grepl("leaf_p", trait) ~ "Leaf P",
      grepl("sla", trait) ~ "SLA",
      grepl("wd", trait) ~ "Wood density",
      TRUE ~ trait) 
    ) %>%
  left_join(., plots[,c("plot_cluster", "clust4")], by = "plot_cluster")

# Plot proportional representation of trees per plot per trait 
pdf(file = "./img/traits_species_cumul.pdf", height = 10, width = 7)
ggplot() + 
  geom_line(data = traits_species_prop_clean,
    aes(x = prop, y = 1 - ..y.., colour = "All"), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  geom_line(data = traits_species_prop_clean,
    aes(x = prop, y = 1 - ..y.., 
      colour = factor(clust4, levels = c("1", "2", "3", "4"), labels = clust_names)), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  scale_colour_manual(values = c("black", clust_pal), name = "Cluster") + 
  facet_grid(trait~method) + 
  geom_vline(xintercept = 0.8, linetype = 2, colour = "red") +
  labs(x = "Proportion of trees", y = "Proportion of plots") + 
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot spatial distribution of trait measurements
pdf(file = "./img/traits_map.pdf", height = 10, width = 14)
ggplot() + 
  geom_sf(data = world) + 
  geom_point(data = traits, 
    aes(x = longitude, y = latitude, fill = trait_id),
    colour = "black", shape = 21) + 
  theme_bw()
dev.off()

# Aggregate stems data to genus
traits_genus_summ <- traits_species_summ %>%
  mutate(genus = unlist(lapply(strsplit(species, " "), "[", 1))) %>%
  dplyr::select(-species) %>%
  group_by(genus) %>%
  summarise(
    across(starts_with("val_std"), ~mean(.x, na.rm = TRUE)),
    across(starts_with("n_meas"), ~sum(.x, na.rm = TRUE))) %>%
  filter(genus %in% stems$genus_clean)

# Add traits to stems dataframe
stems_genus_traits <- left_join(stems, 
  traits_genus_summ %>% 
    dplyr::select(genus, starts_with("val_std")),
  by = c("genus_clean" = "genus"))

# Split stems data by plot
traits_genus_plot_split <- split(stems_genus_traits, stems_genus_traits$plot_cluster)

# For each plot, proportion of trees and ba represented for each trait
traits_genus_prop <- do.call(rbind, lapply(traits_genus_plot_split, function(x) {
    n_trees <- length(unique(x$tree_id))
    ba <- sum(x$ba, na.rm = TRUE)
  data.frame(
    plot_cluster = unique(x$plot_cluster),
    n_trees,
    ba,
    prop_ind_leaf_n = length(unique(x[!is.na(x$val_std_leaf_n), "tree_id"])) / n_trees,
    prop_ind_leaf_p = length(unique(x[!is.na(x$val_std_leaf_p), "tree_id"])) / n_trees,
    prop_ind_sla = length(unique(x[!is.na(x$val_std_sla), "tree_id"])) / n_trees,
    prop_ind_wd = length(unique(x[!is.na(x$val_std_wd), "tree_id"])) / n_trees,
    prop_ba_leaf_n = sum(x[!is.na(x$val_std_leaf_n), "ba"], na.rm = TRUE) / ba ,
    prop_ba_leaf_p = sum(x[!is.na(x$val_std_leaf_p), "ba"], na.rm = TRUE) / ba,
    prop_ba_sla = sum(x[!is.na(x$val_std_sla), "ba"], na.rm = TRUE) / ba,
    prop_ba_wd = sum(x[!is.na(x$val_std_wd), "ba"], na.rm = TRUE) / ba
  )
}))

# Make a clean dataframe
traits_genus_prop_clean <- traits_genus_prop %>%
  gather(trait, prop, -plot_cluster, -n_trees, -ba) %>%
  mutate(
    method = case_when(
      grepl("ind", trait) ~ "N trees",
      grepl("ba", trait) ~ "Basal area",
      TRUE ~ trait),
    trait = case_when(
      grepl("leaf_n", trait) ~ "Leaf N",
      grepl("leaf_p", trait) ~ "Leaf P",
      grepl("sla", trait) ~ "SLA",
      grepl("wd", trait) ~ "Wood density",
      TRUE ~ trait) 
    ) %>%
  left_join(., plots[,c("plot_cluster", "clust4")], by = "plot_cluster")

# Plot proportional representation of trees per plot per trait 
pdf(file = "./img/traits_genus_cumul.pdf", height = 10, width = 7)
ggplot() + 
  geom_line(data = traits_genus_prop_clean,
    aes(x = prop, y = 1 - ..y.., colour = "All"), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  geom_line(data = traits_genus_prop_clean,
    aes(x = prop, y = 1 - ..y.., 
      colour = factor(clust4, levels = c("1", "2", "3", "4"), labels = clust_names)), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  scale_colour_manual(values = c("black", clust_pal), name = "Cluster") + 
  facet_grid(trait~method) + 
  geom_vline(xintercept = 0.8, linetype = 2, colour = "red") +
  labs(x = "Proportion of trees", y = "Proportion of plots") + 
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

##' Reviewer wants at least 80% of basal area in plot with trait measurements
##' Leaf N, Leaf P, Wood density
##' Leaf N and Leaf P to approximate leaf economic spectrum
##' Wood density to approximate wood economic spectrum

##' For plots with at least 80% of individuals with a genus-level measurement for 
##' wood density and Leaf N, are there relationships between community-weighted means 
##' of those traits and AGB? If no we can stop there.

##' While replication probably should be an issue, it's common to have little 
##' replication, so it wouldn't be a convincing argument for reviewers

# find plots: >80% of trees with genus-level measurement for density & Leaf N
good_plots <- traits_genus_prop_clean %>%
  filter(method == "Basal area") %>%
  dplyr::select(plot_cluster, trait, prop) %>%
  filter(trait %in% c("Leaf N", "Wood density")) %>%
  spread(trait, prop) %>%
  filter(
    `Leaf N` >= 0.8,
    `Wood density` >= 0.8) %>%
  pull(plot_cluster)

# Generate community weighted means of traits per plot
stems_cwm <- stems_genus_traits %>% 
  filter(plot_cluster %in% good_plots) %>% 
  group_by(plot_cluster) %>%
  summarise(
    leaf_n_cwm = weighted.mean(val_std_leaf_n, ba, na.rm = TRUE),
    wd_cwm = weighted.mean(val_std_wd, ba, na.rm = TRUE)) %>%
  left_join(., plots[,c("plot_cluster", "clust4", "agb_ha")], by = "plot_cluster")

# Linear regressions of CWMs vs. AGB
mod_list <- list()
mod_list[[1]] <- mod_agb_n <- lm(agb_ha ~ leaf_n_cwm, data = stems_cwm)
mod_list[[2]] <- mod_agb_wd <- lm(agb_ha ~ wd_cwm, data = stems_cwm)
mod_list[[3]] <- mod_agb_n_wd <- lm(agb_ha ~ wd_cwm + leaf_n_cwm, data = stems_cwm)

# Create dataframe of model summary statistics
mod_summ_list <- map_df(mod_list, glance) %>%
  mutate(preds <- c("Leaf N", "Wood dens.", "Leaf N + Wood dens."))

# Write to file
saveRDS(mod_summ_list, "output/cwm_mod.rds")

# Gather CWMs for plotting
stems_cwm_gather <- stems_cwm %>%
  gather(key, val, -plot_cluster, -clust4, -agb_ha) %>%
  mutate(key = case_when(
      grepl("leaf_n", key) ~ "Leaf N",
      grepl("wd", key) ~ "Wood density",
      TRUE ~ key))

# Plot relationship between CWMs and AGB
pdf(file = "img/stems_cwm_agb.pdf", height = 8, width = 10)
ggplot(data = stems_cwm_gather, aes(x = val, y = agb_ha)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~key, scales = "free_x") + 
  theme_bw() + 
  labs(x = "", y = expression("AGB"~"ha"^-1))
dev.off()

# How many of my species are in the traits database?
species_in_traits <- traits_species_summ %>%
  filter(species %in% stems$species_name_clean) %>%
  pull(species) %>%
  unique() %>%
  length()
species_in_traits

# As a proportion of the total number of species in my plots.
species_in_traits / length(unique(stems$species_name_clean))

# How many of my genera are in traits?
genera_in_traits <- traits_genus_summ %>%
  pull(genus) %>%
  unique() %>%
  length()
genera_in_traits

# As a proportion of the total number of genera in my plots.
genera_in_traits / length(unique(stems$genus_clean))

# Gather number of measurements per species
traits_species_n_meas <- traits_species_summ %>%
  filter(species %in% stems$species_name_clean) %>%
  dplyr::select(species, starts_with("n_meas")) %>%
  gather(trait, n, -species) %>%
  filter(!is.na(n)) %>%
  mutate(trait = case_when(
      grepl("leaf_n", trait) ~ "Leaf N",
      grepl("leaf_p", trait) ~ "Leaf P",
      grepl("sla", trait) ~ "SLA",
      TRUE ~ trait)) %>%
  mutate(gen_sp = "Species")

# Gather number of measurements per genus
traits_genus_n_meas <- traits_genus_summ %>%
  dplyr::select(genus, starts_with("n_meas")) %>%
  gather(trait, n, -genus) %>%
  filter(!is.na(n)) %>%
  mutate(trait = case_when(
      grepl("leaf_n", trait) ~ "Leaf N",
      grepl("leaf_p", trait) ~ "Leaf P",
      grepl("sla", trait) ~ "SLA",
      TRUE ~ trait)) %>%
  mutate(gen_sp = "Genus")

# Bind species and genus numbers of measurements together
traits_n_meas_gather <- bind_rows(traits_species_n_meas, traits_genus_n_meas)

# Plot proportion of species with N measurements, cumulative
pdf(file = "./img/traits_freq.pdf", height = 4, width = 12)
ggplot() + 
  geom_line(data = traits_n_meas_gather,
    aes(x = n, y = 1 - ..y..),
    stat = "ecdf", pad = FALSE, size = 1) + 
  facet_grid(gen_sp~trait) + 
  labs(x = "N measurements", y = "Proportion of taxa") + 
  theme_bw() 
dev.off()
