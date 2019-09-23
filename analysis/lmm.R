# Linear Mixed Effects models predicting biomass to determine optimal model structure for SEM
# John Godlee (johngodlee@gmail.com)
# 2019_07_11

# Preamble ----

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(colortools)

# Cluster colour palette
source("clust_pal.R")

# Import data ----

# Aggregated and filtered data
plot_data <- read.csv("data/plot_data_fil_agg_norm_std.csv")

# Make cluster categorical
plot_data$clust5 <- as.character(plot_data$clust5)

# Remove plots not in a cluster
plot_data <- plot_data %>%
  filter(!is.na(clust5))

# Plot all variables against bchave_log in a facet wrap ----

# Climatic variables
plot_data_gather_clim <- plot_data %>%
  select(clust5, bchave_log, aridity_index, mean_temp, temp_seasonality, 
    fire_return_mean_log, firecount_2001_2018,
    ocdens, sand_per, cation_ex_cap) %>%
  gather(key = "variable", value = "value", 3:9) %>%
  mutate(facet_label = factor(variable, 
    levels = c("aridity_index", "mean_temp", "temp_seasonality", 
      "fire_return_mean_log", "firecount_2001_2018", "ocdens", "sand_per"),
    labels = c(
      expression("Aridity" ~ "index"),
      expression("Mean" ~ "annual" ~ "temp." ~ (degree * C)), 
      expression("Temp." ~ "seasonality" ~ (degree * C)),
      expression("log(Mean" ~ "fire" ~ "return" ~ "interval)" ~ (yr)),
      expression("Number" ~ "of" ~ "fires" ~ "2001-18"),
      expression("Organic" ~ "C" ~ "%"),
      expression("Sand" ~ "%"))))


pdf(file = "img/biomass_clim_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_gather_clim, aes(x = value, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) +
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  facet_wrap(~facet_label, scales = "free_x", labeller = label_parsed)
dev.off()

# Diversity variables
plot_data_gather_div <- plot_data %>%
  select(clust5, bchave_log, mean_height, cov_height, mean_dbh_log,
    cov_dbh, stems_ha_log, shannon_cube, sp_rich, sp_rich_raref) %>%
  gather(key = "variable", value = "value", 3:10) %>%
  mutate(facet_label = factor(variable, 
    levels = c("mean_height", "cov_height", "mean_dbh_log", "cov_dbh", 
      "stems_ha_log", "shannon_cube", "sp_rich", "sp_rich_raref"),
    labels = c(
      expression("Mean" ~ "height" ~ (m)),
      expression("Coef." ~ "var." ~ "height"),
      expression("log(Mean" ~ "DBH)" ~ (cm)),
      expression("Coef." ~ "var." ~ "DBH"),
      expression("log(Stem" ~ "density)" ~ ">5" ~ cm ~ (n ~ ha^-1)),
      expression("Shannon" ~ "Index" ~ (H)),
      expression("Species" ~ "richness"),
      expression("Rarefied" ~ "species" ~ "richness")
    )))

pdf(file = "img/biomass_div_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data_gather_div, aes(x = value, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) + 
  theme_classic() + 
  facet_wrap(~variable, scales = "free_x", labeller = label_parsed)
dev.off()

# Investigating species richness vs. biomass
pdf(file = "img/biomass_sp_rich_lm_clust.pdf", width = 12, height = 10)
ggplot(plot_data, aes(x = sp_rich, y = bchave_log)) + 
  geom_point(aes(fill = clust5), colour = "black", shape = 21) + 
  geom_smooth(method = "lm", aes(colour = clust5)) + 
  scale_fill_manual(name = "Cluster", values = clust_pal) + 
  scale_colour_manual(name = "Cluster", values = clust_pal) +
  labs(x = "Species richness", y = expression("AGB" ~ (t ~ ha^-1))) + 
  theme_classic()
dev.off()

# Mixed models ----

# Model specification
model_list <- list(
  model_aridity = lmer(bchave_log ~ aridity_index + (1|clust5), data = plot_data),
  model_temp = lmer(bchave_log ~ mean_temp + (1|clust5), data = plot_data),
  model_temp_s = lmer(bchave_log ~ temp_seasonality + (1|clust5), data = plot_data),
  model_fire_return = lmer(bchave_log ~ fire_return_mean_log + (1|clust5), data = plot_data),
  model_fire_count = lmer(bchave_log ~ firecount_2001_2018 + (1|clust5), data = plot_data),
  
  model_spr = lmer(bchave_log ~ sp_rich + (1|clust5), data = plot_data),
  model_shannon = lmer(bchave_log ~ shannon_cube + (1|clust5), data = plot_data),
  model_stem_n = lmer(bchave_log ~ stems_ha_log + (1|clust5), data = plot_data),
  
  model_cov_height = lmer(bchave_log ~ cov_height + (1|clust5), data = plot_data),
  model_cov_height = lmer(bchave_log ~ cov_dbh + (1|clust5), data = plot_data),
  
  model_ocdens = lmer(bchave_log ~ ocdens + (1|clust5), data = plot_data),
  model_cec = lmer(bchave_log ~ cation_ex_cap + (1|clust5), data = plot_data),
  model_sand_per = lmer(bchave_log ~ sand_per + (1|clust5), data = plot_data)
  )

summ_list <- list()

# Create model visualisations
for(i in 1:length(model_list)){
  summ <- summary(model_list[[i]])
  summ_list[[i]] <- summ
  model_test_pred <- expand.grid(pred = seq(
    from = min(plot_data[,rownames(summ$coefficients)[2]], na.rm = TRUE), 
    to = max(plot_data[,rownames(summ$coefficients)[2]], na.rm = TRUE)),
  clust5 = c("1", "2", "3", "4", "5"), 
  bchave_log = 0)
  colnames(model_test_pred)[1] <- rownames(summ$coefficients)[2]
  
matrix_model_test = model.matrix(terms(model_list[[i]]), data = model_test_pred)
model_test_pred$bchave_log = matrix_model_test %*% fixef(model_list[[i]])
model_test_var <- diag(matrix_model_test %*% tcrossprod(vcov(model_list[[i]]), matrix_model_test))
model_test_pred <- data.frame(model_test_pred, 
  plo = model_test_pred$bchave_log-2*sqrt(model_test_var), 
  phi = model_test_pred$bchave_log+2*sqrt(model_test_var))

print(ggplot(plot_data, aes(x=get(rownames(summ$coefficients)[2]), y=bchave_log)) + 
  geom_point(aes(colour=clust5), alpha = 0.5) +
  geom_smooth(aes(x = get(rownames(summ$coefficients)[2]), y = bchave_log, colour = clust5), 
    method = "lm", se = FALSE) + 
  geom_line(data = model_test_pred, aes(x = get(rownames(summ$coefficients)[2]), y = bchave_log), 
    stat = "identity", position = "identity") + 
  geom_ribbon(data = model_test_pred, aes(x = get(rownames(summ$coefficients)[2]), ymin = plo, ymax = phi), 
    stat = "identity", position = "identity", alpha = 0.2) + 
  theme_classic() +
  scale_color_manual(values = clust_pal) + 
  ggtitle(rownames(summ$coefficients)[2]) + 
  xlab(rownames(summ$coefficients)[2]))

model_test_slope <- plot_data %>%
  group_by(clust5) %>%
  do(mod = lm(bchave_log ~ get(rownames(summ$coefficients)[2]), data = .)) %>%
  mutate(slope = summary(mod)$coeff[2],
    se = summary(mod)$coeff[, 2][2]) %>%
  dplyr::select(-mod)

print(ggplot(model_test_slope, aes(x = clust5)) + 
  geom_point(aes(y = slope, colour = clust5), size = 2) +
  geom_errorbar(aes(ymin = slope - se, ymax = slope + se, colour = clust5), width = 1) +
  geom_hline(yintercept = 0, linetype = 5) + 
  theme_classic() + 
  scale_color_manual(values = clust_pal) + 
  ggtitle(rownames(summ$coefficients)[2]))
}

sink("output/lmm_single_predictor_output.txt")
print(summ_list)
sink() 

