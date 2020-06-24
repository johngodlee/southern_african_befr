# Bivariate relationships between vars., transform, normalise, standardise data
# John Godlee (johngodlee@gmail.com)
# 2018-12-10
# 2020-06-24

# Preamble ----

# Source files
# source("sem_data_collation.R")

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)  #
library(tidyr)  #
library(ggplot2)  #
library(gridExtra)  #
library(lme4)  #
library(MuMIn)  #

source("scripts/clust_defin.R")

# Import data ----

dat <- readRDS("data/plot_data_fil_agg.rds")


# Create histogram labels ----

facet_levels <- c(
  "precip",
  "precip_seas",
  "temp",
  "temp_seas",
  "agb_ha",
  "agb_ha_log", 
  "cec",
  "sand",
  "soil_c",
  "soil_c_log",
  "cov_dbh",
  "cov_height",
  "n_trees_gt10_ha",
  "n_trees_gt10_ha_log",
  "shannon_equit",
  "n_species_raref",
  "n_species_raref_log"
  )

facet_labels <- c(  
  expression("MAP" ~ (mm ~ y^-1)),
  expression("Precip." ~ "seasonality"),
  expression("MAT" ~ (degree * C)), 
  expression("Temp." ~ "seasonality"),
  expression("AGB" ~ (t ~ ha^-1)),
  expression("log(AGB)" ~ (t ~ ha^-1)),
  expression("CEC"),
  expression("Sand" ~ "%"), 
  expression("Org." ~ "C" ~ "(ppt)"),
  expression("log(Org." ~ "C)" ~ "(ppt)"),
  expression("Coef." ~ "var." ~ "DBH"),
  expression("Coef." ~ "var." ~ "height"),
  expression("Stem" ~ "density" ~ ">10" ~ cm ~ (n ~ ha^-1)),
  expression("log(Stem" ~ "density)" ~ ">10" ~ cm ~ (n ~ ha^-1)),
  expression("Shannon" ~ "equit." ~ (E[H])),
  expression("Extrap." ~ "species" ~ "rich."),
  expression("log(Extrap." ~ "species" ~ "rich.)"))

facets <- list(facet_levels = facet_levels, facet_labels = facet_labels)

# Create histogram of raw data ----

hist_raw <- dat %>%
  dplyr::select(agb_ha, n_trees_gt10_ha, 
    n_species_raref, shannon_equit,
    cec, sand, soil_c, 
    precip, precip_seas, temp, temp_seas,
    cov_height, cov_dbh) %>%
  gather(variable, value) %>%
  mutate(facet_label = factor(variable,
    levels = facets[[1]],
    labels = facets[[2]])) %>%
  ggplot(aes(x = value)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  facet_wrap(~facet_label, scales = "free", labeller = label_parsed) + 
  labs(x = "", y = "")

pdf(file = "img/hist_raw.pdf", width = 12, height = 7)
hist_raw
dev.off()


# Transform necessary variables ----

dat_trans <- dat %>%
	mutate(agb_ha_log = log(agb_ha),
		n_trees_gt10_ha_log = log(n_trees_gt10_ha), 
	  n_species_raref_log = log(n_species_raref + 4),
	  soil_c_log = log(soil_c + 4))

hist_trans <- dat_trans %>%
  dplyr::select(agb_ha_log, n_trees_gt10_ha_log,
    n_species_raref_log, shannon_equit,
    cec, sand, soil_c_log,
    precip, precip_seas, temp, temp_seas,
    cov_height, cov_dbh) %>%
  gather(variable, value) %>%
  mutate(facet_label = factor(variable,
    levels = facets[[1]],
    labels = facets[[2]])) %>%
  ggplot(., aes(x = value)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  facet_wrap(~facet_label, scales = "free", labeller = label_parsed) + 
  labs(x = "", y = "")

pdf(file = "img/hist_trans.pdf", width = 12, height = 7)
hist_trans
dev.off()


# Standardize each variable ----
sem_data_norm_std <- dat_trans %>%
	mutate_at(.vars = c(
	  "precip",
	  "precip_seas",
	  "temp",
	  "temp_seas",
	  "agb_ha_log",
	  "cec",
	  "sand",
	  "soil_c_log",
	  "cov_dbh",
	  "cov_height",
	  "n_trees_gt10_ha_log",
	  "shannon_equit",
	  "n_species_raref_log"),
	  .funs = list(std = ~(scale(.) %>% as.vector)))


# Plot bivariate relationships ----

##' Refers to causal paths in SEM conceptual diagram
bivar_list <- c(
  "agb_ha_log_std ~ cec_std",
  "agb_ha_log_std ~ soil_c_log_std",
  "agb_ha_log_std ~ sand_std",
  "agb_ha_log_std ~ cov_dbh_std",
  "agb_ha_log_std ~ cov_height_std",
  "agb_ha_log_std ~ shannon_equit_std",
  "agb_ha_log_std ~ n_species_raref_log_std",
  "agb_ha_log_std ~ temp_std",
  "agb_ha_log_std ~ temp_seas_std",
  "agb_ha_log_std ~ precip_std",
  "agb_ha_log_std ~ precip_seas_std",
  "agb_ha_log_std ~ n_trees_gt10_ha_log_std",
  
  "n_trees_gt10_ha_log_std ~ n_species_raref_log_std",
  "n_trees_gt10_ha_log_std ~ shannon_equit_std",
  "n_trees_gt10_ha_log_std ~ temp_std",
  "n_trees_gt10_ha_log_std ~ temp_seas_std",
  "n_trees_gt10_ha_log_std ~ precip_std",
  "n_trees_gt10_ha_log_std ~ precip_seas_std",
  "n_trees_gt10_ha_log_std ~ cec_std",
  "n_trees_gt10_ha_log_std ~ soil_c_log_std",
  "n_trees_gt10_ha_log_std ~ sand_std",
  
  "n_species_raref_log_std ~ cec_std",
  "n_species_raref_log_std ~ soil_c_log_std",
  "n_species_raref_log_std ~ sand_std",
  "n_species_raref_log_std ~ temp_std",
  "n_species_raref_log_std ~ temp_seas_std",
  "n_species_raref_log_std ~ precip_std",
  "n_species_raref_log_std ~ precip_seas_std")

# Create models
lm_list <- list()
for (i in 1:length(bivar_list)) {
  lm_list[[i]] <- lm(eval(bivar_list[[i]]), data = sem_data_norm_std)
  names(lm_list)[[i]] <- paste0(
    sapply(strsplit(bivar_list, split = " ~ "), function(x){x[1]})[i], 
    "_vs_", 
    sapply(strsplit(bivar_list, split = " ~ "), function(x){x[2]})[i])
}

# Extract model statistics into table
mod_name <- list()
predic <- list()
respon <- list()
f_stat <- list()
dof_lo <- list()
dof_hi <- list()
p_stat <- list()
r2_stat <- list()

for(i in 1:length(lm_list)){
	mod_summ <- summary(lm_list[[i]])
	mod_anova <- anova(lm_list[[i]])
	
	mod_name[[i]] <- names(lm_list)[[i]]
	predic[[i]] <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[2]})[i]
	respon[[i]] <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[1]})[i]
	f_stat[[i]] <- mod_summ$fstatistic[1]
	dof_lo[[i]] <- mod_summ$fstatistic[2]
	dof_hi[[i]] <- mod_summ$fstatistic[3]
	p_stat[[i]] <- mod_anova$"Pr(>F"[1] 
	r2_stat[[i]] <- mod_summ$r.squared
}

bivar_lm_stats <- data.frame(mod_name = unlist(mod_name),
	predic = unlist(predic),
	respon = unlist(respon),
	f_stat = unlist(f_stat),
	dof_lo = unlist(dof_lo),
	dof_hi = unlist(dof_hi),
	p_stat = unlist(p_stat),
	r2_stat = unlist(r2_stat))

bivar_lm_stats %>%
	filter(p_stat > 0.05)

bivar_lm_stats_output <- bivar_lm_stats %>%
	mutate(
		f_stat = round(.$f_stat, digits = 2),
		p_stat = ifelse(.$p_stat > 0.05, 
		round(.$p_stat, digits = 3),
		"<0.05"),
		r2_stat = round(.$r2_stat, digits = 3))

saveRDS(bivar_lm_stats_output, "output/bivar_lm.rds")

# Create plots ----
plot_list = list()
for (i in 1:length(bivar_list)) {
  x <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[2]})[i]
  y <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[1]})[i]
  
	p = ggplot() + 
		geom_point(data = sem_data_norm_std,
		  aes_string(x = x, y = y, 
		    fill = as.factor(sem_data_norm_std$clust4)), 
		  colour = "black", shape = 21, alpha = 0.8) + 
		geom_line(data = sem_data_norm_std,
		  aes_string(x = x, y = y,
		    colour = as.factor(sem_data_norm_std$clust4)), 
			stat = "smooth",
			method = "lm",
			alpha = ifelse(bivar_lm_stats$p_stat[i] > 0.05, 0, 1),  # Don't display linear model if not significant
			se = FALSE) + 
	  geom_line(data = sem_data_norm_std,
	    aes_string(x = x, y = y), 
	    colour = "black",
	    size = 1.5,
	    stat = "smooth",
	    method = "lm",
	    alpha = ifelse(bivar_lm_stats$p_stat[i] > 0.05, 0, 1),  # Don't display linear model if not significant
	    se = FALSE) + 
		geom_line(data = sem_data_norm_std,
		  aes_string(x = x, y = y), 
			stat = "smooth",
			method = "loess",
			colour = "#DE6400",
			alpha = ifelse(bivar_lm_stats$p_stat[i] > 0.05, 0, 1),
			se = FALSE) + 
	  scale_fill_manual(name = "Cluster", values = clust_pal) + 
	  scale_colour_manual(name = "Cluster", values = clust_pal) + 
	  theme(legend.position = "none")
	plot_list[[i]] = p
}

# Arrange on grid
n <- length(plot_list)
n_col <- floor(sqrt(n))

pdf(file =  "img/bivar_lm.pdf", width = 14, height = 10)
do.call("grid.arrange", c(plot_list, ncol = 5))
dev.off()

# Write standardized data ----
saveRDS(sem_data_norm_std, "data/plot_data_fil_agg_norm_std.rds")

# Look at how relationships vary with cluster ----
cluster_compare <- sem_data_norm_std %>%
  group_by(clust4) %>%
  summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE))) %>%
  select_if(function(x){any(!is.na(x))}) %>%
  dplyr::select(-ends_with("_std_mean")) %>%
  dplyr::select(-ends_with("_std_sd")) %>%
  dplyr::select(-starts_with("var_"))
  
cluster_compare_mean <- cluster_compare %>%
  dplyr::select(clust4, ends_with("mean")) %>%
  gather(., key = "variable", value = "mean", ends_with("mean")) %>%
  separate(variable, into = c("variable", "stat"), sep="_(?=[^_]+$)") %>%
  dplyr::select(-stat)

cluster_compare_sd <- cluster_compare %>%
  dplyr::select(clust4, ends_with("sd")) %>%
  gather(., key = "variable", value = "sd", ends_with("sd")) %>%
  separate(variable, into = c("variable", "stat"), sep="_(?=[^_]+$)") %>%
  dplyr::select(-stat)

cluster_compare_gather <- left_join(cluster_compare_mean, cluster_compare_sd, by = c("clust4", "variable"))

cluster_compare_gather$clust4 <- as.character(cluster_compare_gather$clust4)

pdf(file = "img/cluster_variable_bar.pdf", width = 10, height = 7)
ggplot(cluster_compare_gather, aes(x = clust4, y = mean, fill = clust4)) + 
    geom_bar(stat = "identity", colour = "black") + 
    geom_errorbar(aes(x = clust4, ymin = mean - sd, ymax = mean + sd), width = 0.5) + 
  facet_wrap(~variable, scales = "free_y", labeller = label_parsed) + 
  scale_fill_manual(values = clust_pal) + 
  theme(legend.position = "none") + 
  xlab("Cluster")
dev.off()

# Linear mixed models, Random effect of cluster ----

lmer_list <- list()
for (i in 1:length(bivar_list)) {
  lmer_list[[i]] <- lmer(eval(paste0(bivar_list[[i]], " + (1|clust4)")), data = sem_data_norm_std)
  names(lmer_list)[[i]] <- paste0(
    sapply(strsplit(bivar_list, split = " ~ "), function(x){x[1]})[i], 
    "_vs_", 
    sapply(strsplit(bivar_list, split = " ~ "), function(x){x[2]})[i])
}

# Extract model statistics into table
mod_name <- list()
predic <- list()
respon <- list()
f_stat <- list()
t_stat <- list()
r2_stat <- list()

for(i in 1:length(lmer_list)){
  mod_summ <- summary(lmer_list[[i]])
  mod_anova <- anova(lmer_list[[i]])
  
  mod_name[[i]] <- names(lmer_list)[[i]]
  predic[[i]] <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[2]})[i]
  respon[[i]] <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[1]})[i]
  f_stat[[i]] <- mod_anova$`F value`
  t_stat[[i]] <- mod_summ$coefficients[2,3]
  r2_stat[[i]] <- r.squaredGLMM(lmer_list[[i]])[2]
}

bivar_lm_stats <- data.frame(mod_name = unlist(mod_name),
  predic = unlist(predic),
  respon = unlist(respon),
  f_stat = unlist(f_stat),
  t_stat = unlist(t_stat),
  r2_stat = unlist(r2_stat))

bivar_lmer_stats_output <- bivar_lm_stats %>%
  mutate(
    f_stat = round(.$f_stat, digits = 2),
    t_stat = round(.$t_stat, digits = 2),
    r2_stat = round(.$r2_stat, digits = 3))

saveRDS(bivar_lmer_stats_output, "output/bivar_lmer.csv")

