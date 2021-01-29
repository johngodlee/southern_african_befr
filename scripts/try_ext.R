# Extract TRY database values
# John Godlee (johngodlee@gmail.com)
# 2021-01-25

# Packages
library(data.table)
library(dplyr)
library(tibble)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

source("scripts/clust_defin.R")

# Import data
dat <- fread("data/try/13399.txt", 
  header = T, sep = "\t", dec = ".", quote = "", data.table = T)

# Species from SEOSAW
plots <- readRDS("data/plot_data_fil_agg_norm_std.rds")
ab_mat <- readRDS("data/stems_ab_mat.rds")

# All plots represented in abundance matrix?
stopifnot(all(row.names(ab_mat) %in% plots$plot_cluster))

# Gather abundance matrix
ab <- ab_mat %>%
  rownames_to_column("plot_id") %>%
  gather(species, n, -plot_id) %>%
  mutate(genus = unlist(lapply(strsplit(species, " "), "[", 1)))

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

dat_species <- dat_clean %>%
  filter(species %in% unique(ab$species)) %>%
  as.data.frame()

# species ID match
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
  species <- unique(x$species)
  total_ind <- rowSums(ab_mat)
  rep_ind <- rowSums(ab_mat[,species, drop = FALSE])
  prop <- rep_ind / total_ind
  data.frame(plot_id = names(prop), prop, trait_short = unique(x$trait_short))
    }))

stopifnot((nrow(traits_species_prop) / nrow(ab_mat)) == length(traits_species_split))

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
traits_species_prop$clust4 <- factor(plots$clust4[match(traits_species_prop$plot_id, plots$plot_cluster)])
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
    group_by(plot_id) %>%
    summarise(total_ind = sum(n, na.rm = TRUE))
  rep_ind <- ab %>%
    group_by(plot_id) %>%
    filter(genus %in% gen) %>%
    summarise(rep_ind = sum(n, na.rm = TRUE)) %>%
    left_join(., total_ind, by = "plot_id") %>%
    mutate(prop = rep_ind / total_ind,
      trait_short = unique(x$trait_short))

    return(rep_ind)
    }))

stopifnot((nrow(traits_genus_prop) / nrow(ab_mat)) == length(traits_genus_split))

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
traits_genus_prop$clust4 <- factor(plots$clust4[match(traits_genus_prop$plot_id, plots$plot_cluster)])
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
