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

source("scripts/clust_defin.R")

# Import data
dat <- fread("data/try/13399.txt", 
  header = T, sep = "\t", dec = ".", quote = "", data.table = T)

# Species from SEOSAW
plots <- readRDS("data/plot_data_fil_agg_norm_std.rds")
ab_mat <- readRDS("data/trees_ab_mat.rds")
stems <- readRDS("data/stems.rds")

# All plots represented in abundance matrix?
stopifnot(all(row.names(ab_mat) %in% plots$plot_cluster))

# Gather abundance matrix
ab <- ab_mat %>%
  rownames_to_column("plot_cluster") %>%
  gather(species, n, -plot_cluster) %>%
  mutate(genus = unlist(lapply(strsplit(species, " "), "[", 1))) %>%
  filter(n > 0)

# Clean data
dat_clean <- dat %>%
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
    error_risk = ErrorRisk) %>%
  mutate(genus = unlist(lapply(strsplit(species, " "), "[", 1)))

# Filter to only the species in my dataset
dat_species <- dat_clean %>%
  filter(species %in% unique(ab$species)) %>%
  as.data.frame()

# Generate TRY species ID lookup table 
species_id_lookup <- dat_species %>%
  dplyr::select(species_id, species) %>%
  distinct()

stopifnot(all(!duplicated(species_id_lookup$species)))

# Trait ID match
trait_id_lookup <- dat_species %>%
  dplyr::select(trait_id, trait) %>%
  filter(!is.na(trait_id)) %>%
  distinct() %>%
  mutate(
    trait = case_when(
      trait_id %in% c(3115, 3116, 3117) ~ gsub(":.*", "", trait),
      TRUE ~ trait),
    trait_short = case_when(
      trait_id == 14 ~ "Leaf N / dry mass",
      trait_id == 15 ~ "Leaf P / dry mass",
      trait_id == 3115 ~ "SLA",
      trait_id == 3106 ~ "Height",
      trait_id == 37 ~ "Phenology type",
      trait_id == 3117 ~ "SLA",
      trait_id == 38 ~ "Woodiness",
      trait_id == 146 ~ "Leaf C:N",
      trait_id == 24 ~ "Bark thickness",
      trait_id == 324 ~ "Crown max diam.",
      trait_id == 587 ~ "Growth rate",
      trait_id == 12 ~ "Leaf longevity",
      trait_id == 197 ~ "Functional type",
      trait_id == 50 ~ "Leaf N / area",
      trait_id == 51 ~ "Leaf P / area",
      trait_id == 413 ~ "Chl / area",
      trait_id == 3116 ~ "SLA",
      trait_id == 164 ~ "Chl / mass",
      TRUE ~ as.character(trait_id))
  )

stopifnot(all(!duplicated(trait_id_lookup$trait_id)))

# Split by observation
dat_species_split <- split(dat_species, dat_species$obs_id)

# Create clean dataframe of observations
traits_species <- as.data.frame(do.call(rbind, lapply(dat_species_split, function(x) {
  traits <- x[!is.na(x$trait_id),
    c("dataset", "obs_id", "species_id", "trait_id", "val", "unit", 
      "val_std", "unit_std", "error_risk")]
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
  mat <- as.numeric(x[x$key_id == 62, "val_std"])
  if (length(mat) == 0) {
    traits$mat <- NA_real_
  } else {
    traits$mat <- mat
  }
  map <- as.numeric(x[x$key_id == 80, "val_std"])
  if (length(map) == 0) {
    traits$map <- NA_real_
  } else {
    traits$map <- map
  }
  return(traits)
    })))

# Match IDs with values
traits_species$trait <- trait_id_lookup$trait[
  match(traits_species$trait_id, trait_id_lookup$trait_id)]
traits_species$trait_short <- trait_id_lookup$trait_short[
  match(traits_species$trait_id, trait_id_lookup$trait_id)]
traits_species$species <- species_id_lookup$species[
  match(traits_species$species_id, species_id_lookup$species_id)]

# Write to file for interm.
saveRDS(traits_species, file = "data/try_species_clean.rds")

# For each trait, find proportion of individuals in each plot that are represented 
traits_species_split <- split(traits_species, traits_species$trait_id)

traits_species_prop <- do.call(rbind, lapply(traits_species_split, function(x) {
  sp <- unique(x$species)
  total_ind <- ab %>%
    group_by(plot_cluster) %>%
    summarise(total_ind = sum(n, na.rm = TRUE))
  rep_ind <- ab %>%
    group_by(plot_cluster) %>%
    filter(species %in% sp) %>%
    summarise(rep_ind = sum(n, na.rm = TRUE)) %>%
    left_join(., total_ind, by = "plot_cluster") %>%
    mutate(prop = rep_ind / total_ind,
      trait_short = unique(x$trait_short))

    return(rep_ind)
    }))

# Plot representation of individuals per plot per trait as histograms
pdf(file = "./img/traits_species_hist.pdf", height = 10, width = 12)
ggplot() + 
  geom_histogram(data = traits_species_prop,
    aes(x = prop)) + 
  facet_wrap(~trait_short, scales = "free_y") + 
  labs(x = "Proportion of stems", y = "N plots") + 
  ggtitle("Number of plots where a given proportion of stems have species measurements in TRY DB") + 
  theme_bw()
dev.off()

# Same as above but coloured by group
traits_species_prop$clust4 <- factor(plots$clust4[match(traits_species_prop$plot_cluster, plots$plot_cluster)])
pdf(file = "./img/traits_species_hist_clust.pdf", height = 10, width = 12)
ggplot() + 
  geom_histogram(data = traits_species_prop,
    aes(x = prop, fill = clust4)) + 
  scale_fill_manual(values = c(clust_pal), name = "Cluster") + 
  facet_wrap(~trait_short, scales = "free_y") + 
  labs(x = "Proportion of stems", y = "N plots") + 
  ggtitle("Number of plots where a given proportion of stems have species measurements in TRY DB") + 
  theme_bw() 
dev.off()

# Same as above but cumulative
pdf(file = "./img/traits_species_cumul.pdf", height = 10, width = 12)
ggplot() + 
  geom_line(data = traits_species_prop,
    aes(x = prop, y = 1 - ..y..), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  facet_wrap(~trait_short) + 
  labs(x = "Proportion of stems", y = "Proportion of plots") + 
  ggtitle("Cumulative proportion of plots where a given proportion of stems have species measurements in TRY DB") + 
  theme_bw() 
dev.off()

pdf(file = "./img/traits_species_cumul_clust.pdf", height = 10, width = 12)
ggplot() + 
  geom_line(data = traits_species_prop,
    aes(x = prop, y = 1 - ..y.., colour = clust4), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  scale_colour_manual(values = c(clust_pal), name = "Cluster") + 
  facet_wrap(~trait_short) + 
  labs(x = "Proportion of stems", y = "Proportion of plots") + 
  ggtitle("Cumulative proportion of plots where a given proportion of stems have species measurements in TRY DB") + 
  theme_bw() 
dev.off()

# How many of my species are in the traits database?
species_in_traits <- length(unique(traits_species$species))
species_in_traits

# As a proportion of the total number of species in my plots.
species_in_traits / length(ab_mat)

# How many trait measurements does each species have?
traits_sp_freq <- traits_species %>%
  group_by(species, trait_short) %>%
  tally() %>% 
  arrange(desc(n))

pdf(file = "./img/traits_species_species_freq.pdf", height = 10, width = 12)
ggplot() + 
  geom_histogram(data = traits_sp_freq, 
    aes(x = n)) +
  facet_wrap(~trait_short, scales = "free") + 
  labs(x = "N measurements", y = "N species") + 
  ggtitle("Measurements per species") + 
  theme_bw() 
dev.off()

pdf(file = "./img/traits_species_species_cumul.pdf", height = 10, width = 12)
ggplot() + 
  geom_line(data = traits_sp_freq,
    aes(x = n, y = 1 - ..y..),
    stat = "ecdf", pad = FALSE, size = 1) + 
  facet_wrap(~trait_short, scales = "free") + 
  labs(x = "N measurements", y = "Proportion of species") + 
  ggtitle("Measurements per species") + 
  theme_bw() 
dev.off()

# Spatial distribution of trait measurements
world <- ne_countries(scale = "medium", returnclass = "sf")

pdf(file = "./img/traits_map.pdf", height = 10, width = 14)
ggplot() + 
  geom_sf(data = world) + 
  geom_point(data = traits_species, 
    aes(x = longitude, y = latitude, fill = trait_short),
    colour = "black", shape = 21) + 
  theme_bw()
dev.off()

# ============================================
# REPEAT FOR GENUS
# ============================================

dat_genus <- dat_clean %>%
  filter(genus %in% unique(ab$genus)) %>%
  as.data.frame()

# Split by observation
dat_genus_split <- split(dat_genus, dat_genus$obs_id)

# Create clean dataframe of observations
traits_genus <- as.data.frame(do.call(rbind, lapply(dat_genus_split, function(x) {
  traits <- x[!is.na(x$trait_id),
    c("dataset", "obs_id", "genus", "trait_id", "val", "unit", 
      "val_std", "unit_std", "error_risk")]
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
  mat <- as.numeric(x[x$key_id == 62, "val_std"])
  if (length(mat) == 0) {
    traits$mat <- NA_real_
  } else {
    traits$mat <- mat
  }
  map <- as.numeric(x[x$key_id == 80, "val_std"])
  if (length(map) == 0) {
    traits$map <- NA_real_
  } else {
    traits$map <- map
  }
  return(traits)
    })))

# Match IDs with values
traits_genus$trait <- trait_id_lookup$trait[
  match(traits_genus$trait_id, trait_id_lookup$trait_id)]
traits_genus$trait_short <- trait_id_lookup$trait_short[
  match(traits_genus$trait_id, trait_id_lookup$trait_id)]

# Write to file for interm.
saveRDS(traits_genus, file = "data/try_genus_clean.rds")

# For each trait, find proportion of individuals in each plot that are represented 
traits_genus_split <- split(traits_genus, traits_genus$trait_id)

traits_genus_prop <- do.call(rbind, lapply(traits_genus_split, function(x) {
  gen <- unique(x$genus)
  total_ind <- ab %>%
    group_by(plot_cluster) %>%
    summarise(total_ind = sum(n, na.rm = TRUE))
  rep_ind <- ab %>%
    group_by(plot_cluster) %>%
    filter(genus %in% gen) %>%
    summarise(rep_ind = sum(n, na.rm = TRUE)) %>%
    left_join(., total_ind, by = "plot_cluster") %>%
    mutate(prop = rep_ind / total_ind,
      trait_short = unique(x$trait_short))

    return(rep_ind)
    }))

# Plot representation of individuals per plot per trait as histograms
pdf(file = "./img/traits_genus_hist.pdf", height = 10, width = 12)
ggplot() + 
  geom_histogram(data = traits_genus_prop,
    aes(x = prop)) + 
  facet_wrap(~trait_short, scales = "free_y") + 
  labs(x = "Proportion of stems", y = "N plots") + 
  ggtitle("Number of plots where a given proportion of stems have genus measurements in TRY DB") + 
  theme_bw()
dev.off()

# Same as above but coloured by group
traits_genus_prop$clust4 <- factor(plots$clust4[match(traits_genus_prop$plot_cluster, plots$plot_cluster)])
pdf(file = "./img/traits_genus_hist_clust.pdf", height = 10, width = 12)
ggplot() + 
  geom_histogram(data = traits_genus_prop,
    aes(x = prop, fill = clust4)) + 
  scale_fill_manual(values = c(clust_pal), name = "Cluster") + 
  facet_wrap(~trait_short, scales = "free_y") + 
  labs(x = "Proportion of stems", y = "N plots") + 
  ggtitle("Number of plots where a given proportion of stems have genus measurements in TRY DB") + 
  theme_bw() 
dev.off()

# Same as above but cumulative
pdf(file = "./img/traits_genus_cumul.pdf", height = 10, width = 12)
ggplot() + 
  geom_line(data = traits_genus_prop,
    aes(x = prop, y = 1 - ..y..), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  facet_wrap(~trait_short) + 
  labs(x = "Proportion of stems", y = "Proportion of plots") + 
  ggtitle("Cumulative proportion of plots where a given proportion of stems have genus measurements in TRY DB") + 
  theme_bw() 
dev.off()

pdf(file = "./img/traits_genus_cumul_clust.pdf", height = 10, width = 12)
ggplot() + 
  geom_line(data = traits_genus_prop,
    aes(x = prop, y = 1 - ..y.., colour = clust4), 
    stat = "ecdf", pad = FALSE, size = 1) + 
  scale_colour_manual(values = c(clust_pal), name = "Cluster") + 
  facet_wrap(~trait_short) + 
  labs(x = "Proportion of stems", y = "Proportion of plots") + 
  ggtitle("Cumulative proportion of plots where a given proportion of stems have genus measurements in TRY DB") + 
  theme_bw() 
dev.off()

# How many of my genus are in the traits database?
genus_in_traits <- length(unique(traits_genus$genus))
genus_in_traits

# As a proportion of the total number of genus in my plots.
genus_in_traits / length(unique(ab$genus))

# How many trait measurements does each genus have?
traits_genus_freq <- traits_genus %>%
  group_by(genus, trait_short) %>%
  tally() %>% 
  arrange(desc(n))

pdf(file = "./img/traits_genus_genus_freq.pdf", height = 10, width = 12)
ggplot() + 
  geom_histogram(data = traits_genus_freq, 
    aes(x = n)) +
  facet_wrap(~trait_short, scales = "free") + 
  labs(x = "N measurements", y = "N genus") + 
  ggtitle("Measurements per genus") + 
  theme_bw() 
dev.off()

pdf(file = "./img/traits_genus_genus_cumul.pdf", height = 10, width = 12)
ggplot() + 
  geom_line(data = traits_genus_freq,
    aes(x = n, y = 1 - ..y..),
    stat = "ecdf", pad = FALSE, size = 1) + 
  facet_wrap(~trait_short, scales = "free") + 
  labs(x = "N measurements", y = "Proportion of genera") + 
  ggtitle("Measurements per genus") + 
  theme_bw() 
dev.off()

# Spatial distribution of trait measurements
world <- ne_countries(scale = "medium", returnclass = "sf")

pdf(file = "./img/traits_genus_map.pdf", height = 10, width = 14)
ggplot() + 
  geom_sf(data = world) + 
  geom_point(data = traits_genus, 
    aes(x = longitude, y = latitude, fill = trait_short),
    colour = "black", shape = 21) + 
  theme_bw()
dev.off()


# ============================================
# 
# ============================================

##' Reviewer wants at least 80% of individuals in plot with trait measurements
##' Leaf N, Leaf P, Wood density
##' Leaf N and Leaf P to approximate leaf economic spectrum
##' Wood density to approximate wood economic spectrum

##' For plots with at least 80% of individuals with a genus-level measurement for 
##' wood density and Leaf N, are there relationships between community-weighted means 
##' of those traits and AGB? If no we can stop there.

##' While replication probably should be an issue, it's common to have little 
##' replication, so it wouldn't be a convincing argument for reviewers

# Get wood density at genus level
wd_genus <- BIOMASS::wdData %>%
  filter(
    genus %in% unique(ab$genus),
    grepl("africa", region, ignore.case = TRUE)) %>%
  group_by(genus) %>%
  summarise(wd = mean(wd, na.rm = TRUE))

ab$wd <- wd_genus$wd[match(ab$genus, wd_genus$genus)]

# Get Leaf N at genus level
leaf_n_genus <- traits_genus %>%
  filter(
    trait_id == 14,
    genus %in% unique(ab$genus)) %>%
  group_by(genus) %>%
  summarise(leaf_n = mean(val_std, na.rm = TRUE))

ab$leaf_n <- leaf_n_genus$leaf_n[match(ab$genus, leaf_n_genus$genus)]

# find plots: >80% of trees with genus-level measurement for density & Leaf N
ab_split <- split(ab, ab$plot_cluster)

per_ind_trait <- lapply(ab_split, function(x) {
  n_all <- sum(x$n)
  n_traits <- sum(x[!is.na(x$wd) & !is.na(x$leaf_n), "n"], na.rm = TRUE)
  return(n_traits / n_all)
    })

per_ind_trait_df <- data.frame(plot_cluster = names(per_ind_trait), 
  per_trait = unlist(per_ind_trait))
per_ind_trait_df$ge80 <- per_ind_trait_df$per_trait >= 0.8

# How many plots is that?
length(which(per_ind_trait_df$ge80 == TRUE))
##' 669

# Generate community weighted means of traits per plot
stems_cwm <- stems %>% 
  filter(plot_cluster %in% 
    per_ind_trait_df[per_ind_trait_df$ge80 == TRUE, "plot_cluster"]) %>% 
  mutate(ba = pi * (diam/2)^2) %>%
  left_join(., leaf_n_genus, by = c("genus_clean" = "genus")) %>%
  left_join(., wd_genus, by = c("genus_clean" = "genus")) %>%
  group_by(plot_cluster) %>%
  summarise(
    leaf_n_cwm = weighted.mean(leaf_n, ba, na.rm = TRUE),
    wd_cwm = weighted.mean(wd, ba, na.rm = TRUE)) %>%
  left_join(., plots[,c("plot_cluster", "clust4", "agb_ha")], by = "plot_cluster")

# Linear regressions of CWMs vs. AGB
mod_list <- list()
mod_list[[1]] <- mod_agb_n <- lm(agb_ha ~ leaf_n_cwm, data = stems_cwm)
mod_list[[2]] <- mod_agb_wd <- lm(agb_ha ~ wd_cwm, data = stems_cwm)
mod_list[[3]] <- mod_agb_n_wd <- lm(agb_ha ~ wd_cwm + leaf_n_cwm, data = stems_cwm)

mod_summ_list <- purrr::map_df(mod_list, glance)

mod_summ_list$preds <- c("Leaf N", "Wood dens.", "Leaf N + Wood dens.")

saveRDS(mod_summ_list, "output/cwm_mod.rds")

# Plot relationship between CWMs and AGB
stems_cwm_gather <- stems_cwm %>%
  gather(key, val, -plot_cluster, -clust4, -agb_ha)

pdf(file = "img/stems_cwm_agb.pdf", height = 8, width = 10)
ggplot(data = stems_cwm_gather, aes(x = val, y = agb_ha)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~key, scales = "free_x") + 
  theme_bw() + 
  labs(x = "", y = "AGB ha^-1")
dev.off()
