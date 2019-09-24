# Bivariate relationships between variables, transform, normalise and standardize the data
# John Godlee (johngodlee@gmail.com)
# 2018_12_10

# Preamble ----

# Source files
# source("sem_data_collation.R")

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)  # ggplot()
library(psych)  # pairs.panels()
library(gridExtra)  # grid.arrange()
library(lme4)  # lmer()
library(MuMIn)  # r.squared.GLMM()


# Functions
# A ggplot2 theme
theme.bivar <- function(base_size = 12, base_font_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_font_family) %+replace%
    theme(
      axis.ticks = element_line(colour = "black", 
        size = rel(1)),
      axis.text = element_text(angle = 0,
        margin = margin(t = rel(1)),
        size = rel(0.8)),
      axis.title = element_text(size = rel(1), 
        face = "plain"),
      axis.title.x = element_text(angle = 0,
        margin = margin(t = rel(1.2))),
      axis.title.y = element_text(angle = 90, 
        margin = margin(r = rel(1.2))),
      
      plot.margin = unit(rep(0.04, times = 4), 
        units = "native"),
      plot.title = element_text(size = rel(1.8), 
        margin = margin(b = rel(12)),
        hjust = rel(0.05)),
      legend.text = element_text(size = rel(1), 
        face = "italic"),
      legend.title = element_text(size = rel(1.5), 
        vjust = rel(0.8)),
      legend.position = "bottom",
      
      panel.grid.major = element_line(colour="black", 
        size = rel(0.2)),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )}

source("clust_pal.R")

facet_levels <- c(
  "aridity_index",
  "mean_temp",
  "temp_seasonality",
  "bchave",
  "cation_ex_cap",
  "sand_per",
  "ocdens",
  "cov_dbh",
  "cov_height",
  "mean_dbh",
  "mean_height",
  "stems_ha",
  "shannon",
  "sp_rich",
  "sp_rich_raref",
  "fire_return_mean",
  "firecount_2001_2018",
  "bchave_log", 
  "mean_dbh_log",
  "stems_ha_log", 
  "shannon_cube",
  "fire_return_mean_log",
  "aridity_index_std",
  "mean_temp_std",
  "temp_seasonality_std", 
  "bchave_log_std", 
  "cation_ex_cap_std",
  "sand_per_std", 
  "ocdens_std", 
  "cov_dbh_std", 
  "cov_height_std", 
  "mean_dbh_log_std",
  "mean_height_std", 
  "stems_ha_log_std", 
  "shannon_cube_std",
  "sp_rich_std", 
  "sp_rich_raref_std", 
  "fire_return_mean_log_std",
  "firecount_2001_2018_std")

facet_labels <- c(  
  expression("Aridity" ~ "index"),
  expression("Mean" ~ "annual" ~ "temp." ~ (degree * C)), 
  expression("Temp." ~ "seasonality" ~ (degree * C)),
  expression("AGB" ~ t ~ ha^-1),
  expression("Cation" ~ "exchange" ~ "capacity"),
  expression("Sand" ~ "%"), 
  expression("Organic" ~ "C" ~ "%"),
  expression("Coef." ~ "var." ~ "DBH"),
  expression("Coef." ~ "var." ~ "height"),
  expression("Mean" ~ "DBH" ~ (cm)),
  expression("Mean" ~ "height" ~ (m)),
  expression("Stem" ~ "density" ~ ">5" ~ cm ~ (n ~ ha^-1)),
  expression("Shannon" ~ "Index" ~ (H)),
  expression("Species" ~ "richness"),
  expression("Rarefied" ~ "species" ~ "richness"),
  expression("Mean" ~ "fire" ~ "return" ~ "interval" ~ (yr)),
  expression("Number" ~ "of" ~ "fires" ~ "2001-18"),
  expression("log(Woody" ~ "biomass)" ~ ha^-1),
  expression("log(Mean" ~ "DBH)" ~ (cm)),
  expression("log(Stem" ~ "density)" ~ ">5" ~ cm ~ (n ~ ha^-1)),
  expression("Shannon" ~ "Index" ~ (H^3)),
  expression("log(Mean" ~ "fire" ~ "return" ~ "interval)" ~ (yr)),
  expression("std(Aridity" ~ "index)"),
  expression("std(Mean" ~ "annual" ~ "temp.)" ~ (degree * C)), 
  expression("std(Temp." ~ "seasonality)" ~ (degree * C)),
  expression("std(log(Woody" ~ "biomass))" ~ ha^-1),
  expression("std(Cation" ~ "exchange" ~ "capacity)"),
  expression("std(Sand" ~ "%)"), 
  expression("std(Organic" ~ "C" ~ "%)"),
  expression("std(Coef." ~ "var." ~ "DBH)"),
  expression("std(Coef." ~ "var." ~ "height)"),
  expression("std(log(Mean" ~ "DBH))" ~ (cm)),
  expression("std(Mean" ~ "height)" ~ (m)),
  expression("std(log(Stem" ~ "density))" ~ ">5" ~ cm ~ (n ~ ha^-1)),
  expression("std(Shannon" ~ "Index)" ~ (H)),
  expression("std(Species" ~ "richness)"),
  expression("std(Rarefied" ~ "species)" ~ "richness"),
  expression("std(log(Mean" ~ "fire" ~ "return" ~ "interval))" ~ (yr)),
  expression("std(Number" ~ "of" ~ "fires" ~ "2001-18)"))



# Import data ----

sem_data_fil_agg <- read.csv("data/plot_data_fil_agg.csv")


# Which variables need to be log transformed? ----
##' Look at histograms
histogram_raw <- sem_data_fil_agg %>%
	select(stems_ha, bchave, sp_rich, sp_rich_raref, shannon, cation_ex_cap, sand_per, ocdens, aridity_index, 
		mean_height, cov_height, mean_dbh, cov_dbh, fire_return_mean, firecount_2001_2018, mean_temp, temp_seasonality) %>%
	gather(variable, value) %>%
	mutate(facet_label = factor(variable,
	  levels = facet_levels,
	  labels = facet_labels)) %>%
	ggplot(aes(x = value)) + 
	geom_histogram(colour = "black", fill = "grey") + 
	facet_wrap(~facet_label, scales = "free", labeller = label_parsed) + 
	theme.bivar() + 
	labs(x = "", y = "")
	
pdf(file = "img/histogram_raw.pdf", width = 12, height = 7)
histogram_raw
dev.off()
##' bchave
##' mean_dbh
##' stems_ha
##' shannon - Not right-skewed, transform != log
##' sp_rich - But this is count data
##' Fire return interval
##' Number of fires 2001-2018
##' Organic C %

sem_data_norm <- sem_data_fil_agg %>%
	mutate(bchave_log = log(bchave),
		mean_dbh_log = log(mean_dbh),
		stems_ha_log = log(stems_ha), 
		shannon_cube = shannon^3, # Log doesn't work to normalise the data
		sp_rich = sp_rich,
		fire_return_mean_log = log(fire_return_mean),
	  ocdens = ocdens) %>% 
	select(-bchave, -mean_dbh, -stems_ha, -shannon, -fire_return_mean)

histogram_trans <- sem_data_norm %>%
	select(stems_ha_log, bchave_log, sp_rich, sp_rich_raref, shannon_cube, 
		cation_ex_cap, sand_per, ocdens, aridity_index,
		mean_height, cov_height, mean_dbh_log, cov_dbh, 
		fire_return_mean_log, mean_temp, temp_seasonality, firecount_2001_2018) %>%
	gather(variable, value) %>%
	mutate(facet_label = factor(variable,
	  levels = facet_levels,
	  labels = facet_labels)) %>%
	ggplot(., aes(x = value)) + 
	geom_histogram(colour = "black", fill = "grey") + 
	facet_wrap(~facet_label, scales = "free", labeller = label_parsed) + 
	theme.bivar() +
	labs(x = "", y = "")

pdf(file = "img/histogram_trans.pdf", width = 12, height = 7)
histogram_trans
dev.off()

# Standardize each variable
sem_data_norm_std <- sem_data_norm %>%
	mutate_at(.vars = c("aridity_index",
		"mean_temp",
		"temp_seasonality",
		"bchave_log",
		"cation_ex_cap",
		"sand_per",
		"ocdens",
		"cov_dbh",
		"cov_height",
		"mean_dbh_log",
		"mean_height",
		"stems_ha_log",
		"shannon_cube",
		"sp_rich",
	  "sp_rich_raref",
		"fire_return_mean_log",
	  "firecount_2001_2018"),
		.funs = list(std = ~(scale(.) %>% as.vector)))

histogram_trans_std <- sem_data_norm_std %>%
	select(stems_ha_log_std, bchave_log_std, sp_rich_std, sp_rich_raref_std, shannon_cube_std, 
	  cation_ex_cap_std, sand_per_std, ocdens_std, aridity_index_std,
		mean_height_std, cov_height_std, mean_dbh_log_std, cov_dbh_std, 
	  fire_return_mean_log_std, mean_temp_std, temp_seasonality_std, firecount_2001_2018_std) %>%
	gather(variable, value) %>%
  mutate(facet_label = factor(variable,
    levels = facet_levels,
    labels = facet_labels)) %>%
	ggplot(., aes(x = value)) + 
	geom_histogram(colour = "black", fill = "grey") + 
	facet_wrap(~facet_label, scales = "free", labeller = label_parsed) + 
	theme.bivar() +
	labs(x = "", y = "")

pdf(file = "img/histogram_trans_std.pdf", width = 12, height = 7)
histogram_trans_std
dev.off()
 
# Which bivariate relationships should be plotted? ----
##' Refers to causal paths in SEM conceptual diagram
bivar_list <- c(
  "bchave_log_std ~ cation_ex_cap_std",
  "bchave_log_std ~ ocdens_std",
  "bchave_log_std ~ cov_dbh_std",
  "bchave_log_std ~ cov_height_std",
  "bchave_log_std ~ shannon_cube_std",
  "bchave_log_std ~ sp_rich_raref_std",
  "bchave_log_std ~ fire_return_mean_log_std",
  "bchave_log_std ~ aridity_index_std",
  "bchave_log_std ~ mean_temp_std",
  "bchave_log_std ~ temp_seasonality_std",
  "bchave_log_std ~ stems_ha_log_std",
  "cov_height_std ~ cation_ex_cap_std",
  "cov_height_std ~ aridity_index_std",
  "cov_height_std ~ fire_return_mean_log_std",
  "cov_height_std ~ shannon_cube_std",
  "cov_height_std ~ sp_rich_raref_std",
  "cov_height_std ~ ocdens_std",
  "cov_dbh_std ~ cation_ex_cap_std",
  "cov_dbh_std ~ aridity_index_std",
  "cov_dbh_std ~ fire_return_mean_log_std",
  "cov_dbh_std ~ shannon_cube_std",
  "cov_dbh_std ~ sp_rich_raref_std",
  "cov_dbh_std ~ ocdens_std")

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

write.csv(bivar_lm_stats_output, "output/bivar_lm.csv", row.names = FALSE)

# Create plots ----
plot_list = list()
for (i in 1:length(bivar_list)) {
  x <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[2]})[i]
  y <- sapply(strsplit(bivar_list, split = " ~ "), function(x){x[1]})[i]
  
	p = ggplot() + 
		geom_point(data = sem_data_norm_std,
		  aes_string(x = x, y = y, 
		    fill = as.factor(sem_data_norm_std$clust5)), 
		  colour = "black", shape = 21, alpha = 0.8) + 
		geom_line(data = sem_data_norm_std,
		  aes_string(x = x, y = y,
		    colour = as.factor(sem_data_norm_std$clust5)), 
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
	  theme.bivar() + 
	  theme(legend.position = "none") + 
	  labs(x = facet_labels[grep(paste0("\\b", x, "\\b"), facet_levels)],
	    y = facet_labels[grep(paste0("\\b", y, "\\b"), facet_levels)])
	plot_list[[i]] = p
}

# Arrange on grid
n <- length(plot_list)
n_col <- floor(sqrt(n))

pdf(file = "img/bivariate_relationships.pdf", width = 14, height = 10)
do.call("grid.arrange", c(plot_list, ncol = 5))
dev.off()

# Save standardized data to csv ----

write.csv(sem_data_norm_std, file = "data/plot_data_fil_agg_norm_std.csv", row.names = FALSE)

# Look at how relationships vary with cluster ----
cluster_compare <- data.frame(
  bchave_log = sem_data_norm_std$bchave_log,
  sp_rich = sem_data_norm_std$sp_rich,
  sp_rich_raref = sem_data_norm_std$sp_rich_raref,
  shannon_cube = sem_data_norm_std$shannon_cube,
  cation_ex_cap = sem_data_norm_std$cation_ex_cap,
  sand_per = sem_data_norm_std$sand_per,
  ocdens = sem_data_norm_std$ocdens, 
  aridity_index = sem_data_norm_std$aridity_index,
  mean_temp = sem_data_norm_std$mean_temp,
  temp_seasonality = sem_data_norm_std$temp_seasonality,
  mean_height = sem_data_norm_std$mean_height,
  stems_ha_log = sem_data_norm_std$stems_ha_log,
  cov_height = sem_data_norm_std$cov_height,
  mean_dbh_log = sem_data_norm_std$mean_dbh_log,
  cov_dbh = sem_data_norm_std$cov_dbh,
  fire_return_mean_log = sem_data_norm_std$fire_return_mean_log,
  firecount_2001_2018 = sem_data_norm_std$firecount_2001_2018,
  clust5 = sem_data_norm_std$clust5) %>%
  group_by(clust5) %>%
  drop_na() %>%
  summarise_all(list(~mean(.), ~sd(.))) 

cluster_compare_mean <- cluster_compare %>%
  select(clust5, ends_with("mean")) %>%
  gather(., key = "variable", value = "mean", ends_with("mean")) %>%
  separate(variable, into = c("variable", "stat"), sep="_(?=[^_]+$)") %>%
  select(-stat)

cluster_compare_sd <- cluster_compare %>%
  select(clust5, ends_with("sd")) %>%
  gather(., key = "variable", value = "sd", ends_with("sd")) %>%
  separate(variable, into = c("variable", "stat"), sep="_(?=[^_]+$)") %>%
  select(-stat)

cluster_compare_gather <- left_join(cluster_compare_mean, cluster_compare_sd, by = c("clust5", "variable"))

cluster_compare_gather$sd_hi <- cluster_compare_gather$mean + cluster_compare_gather$sd
cluster_compare_gather$sd_lo <- cluster_compare_gather$mean - cluster_compare_gather$sd

cluster_compare_gather$clust5 <- as.character(cluster_compare_gather$clust5)

cluster_compare_gather <- cluster_compare_gather %>%
  mutate(facet_label = factor(variable,
    levels = c("aridity_index",
      "mean_temp",
      "temp_seasonality", 
      "bchave_log", 
      "cation_ex_cap",
      "sand_per", 
      "ocdens", 
      "cov_dbh", 
      "cov_height", 
      "mean_dbh_log",
      "mean_height", 
      "stems_ha_log", 
      "shannon_cube",
      "sp_rich", 
      "sp_rich_raref", 
      "fire_return_mean_log",
      "firecount_2001_2018"),
    labels = c(
      expression("Aridity" ~ "index"),
      expression("Mean" ~ "annual" ~ "temp." ~ (degree * C)), 
      expression("Temp." ~ "seasonality" ~ (degree * C)),
      expression("log(Woody" ~ "biomass)" ~ ha^-1),
      expression("Cation" ~ "exchange" ~ "capacity"),
      expression("Sand" ~ "%"), 
      expression("Organic" ~ "C" ~ "%"),
      expression("Coef." ~ "var." ~ "DBH"),
      expression("Coef." ~ "var." ~ "height"),
      expression("log(Mean" ~ "DBH)" ~ (cm)),
      expression("Mean" ~ "height" ~ (m)),
      expression("log(Stem" ~ "density)" ~ ">5" ~ cm ~ (n ~ ha^-1)),
      expression("Shannon" ~ "Index" ~ (H)),
      expression("Species" ~ "richness"),
      expression("Rarefied" ~ "species" ~ "richness"),
      expression("log(Mean" ~ "fire" ~ "return" ~ "interval)" ~ (yr)),
      expression("Number" ~ "of" ~ "fires" ~ "2001-18"))))

pdf(file = "img/cluster_variable_bar.pdf", width = 10, height = 7)
ggplot(cluster_compare_gather, aes(x = clust5, y = mean, fill = clust5)) + 
    geom_bar(stat = "identity", colour = "black") + 
    geom_errorbar(aes(x = clust5, ymin = sd_lo, ymax = sd_hi), width = 0.5) + 
  facet_wrap(~facet_label, scales = "free_y", labeller = label_parsed) + 
  scale_fill_manual(values = clust_pal) + 
  theme.bivar() + 
  theme(legend.position = "none") + 
  xlab("Cluster")
dev.off()

# Linear mixed models, Random effect of cluster ----

lmer_list <- list()
for (i in 1:length(bivar_list)) {
  lmer_list[[i]] <- lmer(eval(paste0(bivar_list[[i]], " + (1|clust5)")), data = sem_data_norm_std)
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

write.csv(bivar_lmer_stats_output, "output/bivar_lmer.csv", row.names = FALSE)
