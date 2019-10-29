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
library(gridExtra)

# Cluster colour palette
source("clust_pal.R")

# Import data ----

# Aggregated and filtered data
plot_data <- read.csv("data/plot_data_fil_agg_norm_std.csv")

plot_data$clust5 <- as.character(plot_data$clust5)

# Mixed models ----
# Model specification
model_list <- list(
  model_precip = lmer(bchave_log ~ total_precip_std + (1|clust5), data = plot_data),
  model_precip_s = lmer(bchave_log ~ precip_seasonality_log_std + (1|clust5), data = plot_data),
  
  model_temp = lmer(bchave_log ~ mean_temp_std + (1|clust5), data = plot_data),
  model_temp_s = lmer(bchave_log ~ temp_seasonality_log_std + (1|clust5), data = plot_data),
  
  model_fire_return = lmer(bchave_log ~ fire_return_mean_log_std + (1|clust5), data = plot_data),

  model_spr = lmer(bchave_log ~ sp_rich_raref_log_std + (1|clust5), data = plot_data),
  model_shannon = lmer(bchave_log ~ shannon_log_std + (1|clust5), data = plot_data),
  model_equit = lmer(bchave_log ~ shannon_equit_log_std + (1|clust5), data = plot_data),
  model_stem_n = lmer(bchave_log ~ stems_ha_log_std + (1|clust5), data = plot_data),
  
  model_cov_height = lmer(bchave_log ~ cov_height_std + (1|clust5), data = plot_data),
  model_cov_height = lmer(bchave_log ~ cov_dbh_std + (1|clust5), data = plot_data),
  
  model_ocdens = lmer(bchave_log ~ ocdens_std + (1|clust5), data = plot_data),
  model_sand_per = lmer(bchave_log ~ sand_per_std + (1|clust5), data = plot_data),
  model_cec = lmer(bchave_log ~ cation_ex_cap_std + (1|clust5), data = plot_data)
  )

# Lists to fill with model output
summ_list <- list()
scatter_list <- list()
slope_list <- list()

# Create model visualisations and model summaries
for(i in 1:length(model_list)){
  summ <- summary(model_list[[i]])
  summ_list[[i]] <- summ
  
  x_var <- rownames(summ$coefficients)[2]
  
  model_test_pred <- expand.grid(pred = seq(
    from = min(plot_data[,x_var], na.rm = TRUE), 
    to = max(plot_data[,x_var], na.rm = TRUE)),
  clust5 = c("1", "2", "3", "4", "5"), 
  bchave_log = 0)
  colnames(model_test_pred)[1] <- x_var
  
  matrix_model_test = model.matrix(terms(model_list[[i]]), data = model_test_pred)
  model_test_pred$bchave_log = matrix_model_test %*% fixef(model_list[[i]])
  model_test_var <- diag(matrix_model_test %*% tcrossprod(vcov(model_list[[i]]), matrix_model_test))
  model_test_pred <- data.frame(model_test_pred, 
    plo = model_test_pred$bchave_log-2*sqrt(model_test_var), 
    phi = model_test_pred$bchave_log+2*sqrt(model_test_var))

  scatter_list[[i]] <- ggplot_gtable(ggplot_build(ggplot() + 
    geom_point(
      aes(x = plot_data[,x_var], y = plot_data$bchave_log, colour = plot_data$clust5), 
      alpha = 0.5) +
    geom_smooth(
      aes(x = plot_data[,x_var], y = plot_data$bchave_log, colour = plot_data$clust5), 
      method = "lm", se = FALSE) + 
    geom_line(
      aes(x = model_test_pred[,x_var], y = model_test_pred$bchave_log), 
      stat = "identity", position = "identity") + 
    geom_ribbon(
      aes(x = model_test_pred[,x_var], ymin = model_test_pred$plo, ymax = model_test_pred$phi), 
      stat = "identity", position = "identity", alpha = 0.2) + 
    theme_classic() +
    scale_color_manual(values = clust_pal, name = "Cluster") + 
    ggtitle(" ") + 
    xlab(rownames(summ$coefficients)[2]) + 
    ylab("bchave_log")))

  model_test_slope <- plot_data %>%
    group_by(clust5) %>%
   do(mod = lm(bchave_log ~ get(x_var), data = .)) %>%
    mutate(slope = summary(mod)$coeff[2],
      se = summary(mod)$coeff[, 2][2]) %>%
    dplyr::select(-mod)

  slope_list[[i]] <- ggplot_gtable(ggplot_build(ggplot(model_test_slope, aes(x = clust5)) + 
    geom_point(aes(y = slope, colour = clust5), size = 2) +
    geom_errorbar(aes(ymin = slope - se, ymax = slope + se, colour = clust5), width = 1) +
    geom_hline(yintercept = 0, linetype = 5) + 
    theme_classic() + 
    theme(legend.position = "none") + 
    scale_color_manual(values = clust_pal) + 
    ggtitle(x_var)))
  
  pdf(file = paste0("img/lmm_slope_", x_var, ".pdf"), width = 10, height = 5) 
  grid.arrange(slope_list[[i]], scatter_list[[i]], nrow = 1)
  dev.off()
}

# Plot all plots in a single grid
plot_list <- Map(list, slope_list, scatter_list)
plot_unlist <- unlist(plot_list, recursive = FALSE)

pdf(file = "img/lmm_slope_scatter_all.pdf", width = 10, height = 50)
do.call("grid.arrange", c(plot_unlist, ncol = 2, nrow = 14))
dev.off()

# Save model summaries as text files
sink("output/lmm_single_predictor_output.txt")
print(summ_list)
sink() 

