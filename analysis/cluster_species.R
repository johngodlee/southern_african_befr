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
library(tidyr)

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
    !is.na(gen_sp)) %>%
  left_join(., select(plot_data, plot_group, clust4), by = "plot_group")

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
clust4_vec <- plot_data$clust4

# Run Dufrene-Legendre indicator species analysis
clust_indval <- indval(ab_mat, clustering = clust4_vec)


sink("output/clust_species_indicators.txt")
summary(clust_indval, p = 0.05, type = "short", digits = 2, show = p)
sink()

# Extract indicator species per cluster
clust_indval$indval$sp <- row.names(clust_indval$indval)
row.names(clust_indval$indval) <- seq(from = 1, to = length(clust_indval$indval$sp))

for(i in 1:4){
  print(head(clust_indval$indval[order(clust_indval$indval[[i]], decreasing = TRUE),c(5, i)]))
}

fileConn<-file("output/clust_species_ind_summary.txt")
writeLines(c(
  "Cluster 1 - Diplorhynchus-Burkea",
  "Diplorhynchus.condylocarpon, Burkea.africana, Pseudolachnostylis.maprouneifolia",
  "", 
  "Cluster 2 - Miombo",
  "Julbernardia.paniculata, Isoberlinia.angolensis, Albizia.antunesiana, Brachystegia.longifolia",
  "",
  "Cluster 3 - Burkea-Senegalia",
  "Baikiaea.plurijuga, Senegalia.ataxacantha, Combretum.collinum",
  "",
  "Cluster 4 - Mopane",
  "Colophospermum mopane"
  ), fileConn)
close(fileConn)

# Find dominant species by biomass per cluster
clust_dom <- s_fil %>%
  group_by(clust4, gen_sp) %>%
  summarise(bchave_total = sum(Bchave))

clust_dom_list <- split(clust_dom, clust_dom$clust4)

for(i in 1:4){
  print(head(clust_dom_list[[i]][order(clust_dom_list[[i]]$bchave_total, decreasing = TRUE),]))
}

fileConn <- file("output/clust_species_dom_summary.txt")
writeLines(c(
  "Cluster 1 - Diplorhynchus-Burkea",
  "Julbernadia.spp., Brachystegia.spiciformis, Baikiaea.plurijuga",
  "",
  "Cluster 2 - Miombo",
  "Julbernadia.spp., Brachystegia.spp., Isoberlinia.angolensis",
  "",
  "Cluster 3 - Spirostachys-Senegalia",
  "Spirostachys.africana, Senegalia.spp., Euclea.racemosa",
  "",
  "Cluster 4 - Mopane",
  "Colophospermum.mopane"
), fileConn)
close(fileConn)

# Build a tidy table describing vegetation clusters
c_name <- c("Baikiaea-Brachystegia", "Miombo", "Spirostachys-Senegalia", "Mopane")
c_ind <- c(
  "Diplorhynchus condylocarpon, Burkea africana, Pseudolachnostylis maprouneifolia",
  "Julbernardia paniculata, Isoberlinia angolensis, Brachystegia longifolia",
  "Baikiaea plurijuga, Senegalia ataxacantha, Combretum collinum",
  "Colophospermum mopane, Combretum spp.")
c_dom <- c(
  "Julbernadia spp., Brachystegia spiciformis, Baikeaea plurijuga",
  "Julbernadia spp., Brachystegia spp., Isoberlinia angolensis",
  "Spirostachys africana, Senegalia spp., Euclea racemosa",
  "Colophospermum mopane")

clust_summ <- plot_data %>%
  group_by(clust4) %>%
  mutate(bchave = exp(bchave_log),
    n_stems_ha = n_stems / area_of_plot) %>%
  summarise(
    n_plots = n(),
    bchave_min = round(min(bchave, na.rm = TRUE), digits = 3),
    bchave_max = round(max(bchave, na.rm = TRUE), digits = 3),
    bchave_median = round(mean(bchave, na.rm = TRUE), digits = 1),
    bchave_sd = round(sd(bchave, na.rm = TRUE), digits =  2),
    bchave_iqr = round(IQR(bchave, na.rm = TRUE), digits = 2),
   
    sp_rich_raref_median = round(mean(sp_rich, na.rm = TRUE), digits = 0),
    sp_rich_raref_sd = round(sd(sp_rich, na.rm = TRUE), digits = 1),
    sp_rich_raref_iqr = round(IQR(sp_rich, na.rm = TRUE), digits = 1),
    
    n_stems_ha_median = round(mean(n_stems_ha, na.rm = TRUE), digits = 0),
    n_stems_ha_iqr = round(IQR(n_stems_ha, na.rm = TRUE), digits = 1),
    n_stems_ha_sd = round(sd(n_stems_ha, na.rm = TRUE), digits = 1)) %>%
  mutate(c_name, c_ind, c_dom,
    clust5 = paste0("C", clust4),
    bchave = paste0(bchave_median, "(", bchave_iqr, ")"),
    sp_rich_raref = paste0(sp_rich_raref_median, "(", sp_rich_raref_iqr, ")"),
    n_stems_ha = paste0(n_stems_ha_median, "(", n_stems_ha_iqr, ")")) %>%
  select(clust4, c_dom, c_ind,  
    n_plots, sp_rich_raref, n_stems_ha, bchave)

fileConn <- file("output/include/clust_summ.tex")
writeLines(stargazer(clust_summ, 
  summary = FALSE,
  label = "clust_summ", digit.separate = 0, rownames = FALSE), fileConn)
close(fileConn)


