# Which species make up the vegetation clusters?
# John Godlee (johngodlee@gmail.com)
# 2019_10_24

# Preamble ----
# Remove old crap
rm(list=ls())
#dev.off()

# Packages ----
library(labdsv)
library(stargazer)
library(dplyr)

# Import data ----
plot_data <- read.csv("data/plot_data_fil_agg_norm_std.csv")
plotcode_plot_group_lookup <- read.csv("data/plotcode_plot_group_lookup.csv")
load("data/clean_input_data.Rdata")

# Create community composition matrix of suitable plots
s_fil <- s %>%
  left_join(., plotcode_plot_group_lookup, by = c("plotcode" = "plotcode_vec")) %>%
  inner_join(., data.frame(plot_group = plot_data$plot_group), 
    by = c("plot_group" = "plot_group")) %>%
  filter(alive %in% c("A", NA),
    !is.na(gen_sp))

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
clust5_vec <- plot_data$clust5

# Run Dufrene-Legendre indicator species analysis
clust_indval <- indval(ab_mat, clustering = clust5_vec)

sink("output/clust_species_indicators.txt")
print(summary(clust_indval, p = 0.05, type = "short", digits = 2, show = p))
sink()

fileConn<-file("output/clust_species_summary.txt")
writeLines(c(
  "Cluster 1 - Diplorhynchus-Combretum",
  "Combretum.zeyheri, Brachystegia.floribunda, Brachystegia.utilis",
  "", 
  "Cluster 2 - Miombo",
  "Julbernardia.paniculata, Isoberlinia.angolensis, Albizia.antunesiana, Brachystegia.longifolia",
  "",
  "Cluster 3 - Burkea-Pterocarpus",
  "Burkea.africana, Pterocarpus.angolensis, Baikiaea.plurijuga",
  "",
  "Cluster 4 - Baikiaea",
  "Baikiaea.plurijuga, Terminalia.randii, Albizia.amara",
  "",
  "Cluster 5 - Mopane",
  "Colophospermum mopane"
  ), fileConn)
close(fileConn)

# Build a tidy table describing vegetation clusters
c_name <- c("Diplorhynchus-Combretum", "Miombo", "Burkea-Pterocarpus", "Baikiaea", "Mopane")
c_ind <- c("Diplorhynchus condylocarpon, Combretum spp., Pseudolachnostylis maprouneifolia",
  "Julbernardia paniculata, Isoberlinia angolensis, Albizia antunesiana",
  "Burkea africana, Pterocarpus angolensis, Baikiaea plurijuga",
  "Baikiaea plurijuga, Terminalia randii, Albizia amara",
  "Colophospermum mopane, Pseudolachnostylis maprouneifolia")

clust_summ <- plot_data %>%
  group_by(clust5) %>%
  mutate(bchave = exp(bchave_log),
    n_stems_ha = n_stems / area_of_plot) %>%
  summarise(bchave_mean = round(mean(bchave, na.rm = TRUE), digits = 1),
    bchave_sd = round(sd(bchave, na.rm = TRUE), digits=  2),
    sp_rich_mean = round(mean(sp_rich, na.rm = TRUE), digits = 0),
    sp_rich_sd = round(sd(sp_rich, na.rm = TRUE), digits = 1),
    n_stems_ha_mean = round(mean(n_stems_ha, na.rm = TRUE), digits = 0),
    n_stems_ha_sd = round(sd(n_stems_ha, na.rm = TRUE), digits = 1)) %>%
  mutate(c_name, c_ind,
    clust5 = paste0("C", clust5),
    bchave = paste0(bchave_mean, "+", bchave_sd),
    sp_rich_raref = paste0(sp_rich_raref_mean, "+", sp_rich_raref_sd),
    n_stems_ha = paste0(n_stems_ha_mean, "+", n_stems_ha_sd)) %>%
  select(clust5, c_ind, sp_rich_raref, n_stems_ha, bchave)


fileConn <- file("output/include/clust_summ.tex")
writeLines(stargazer(clust_summ, 
  summary = FALSE,
  label = "clust_summ", digit.separate = 0, rownames = FALSE), fileConn)
close(fileConn)


