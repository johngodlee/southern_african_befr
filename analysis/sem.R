# Structural Equation Models for Chapter 1 - Regional analysis of the factors affecting woody AGB and productivity
# John Godlee (johngodlee@gmail.com)
# 2019_09_20

# Notes for interpreting SEM models
##' Double headed arrows show covariances
##' Single headed arrows show causal relationships
##' Exogenous variables (i.e. independent variables) should have a measurement error (rounded double ended arrows) of 1.00. Exogenous variables may have covariance

# Notes for constructing SEM model specifications
##' ~ regressed by
##' =~ Latent variable
##' <~ Composite variable
##' ~~ Correlated with

# Preamble ----

# Source files
# source("sem_data_collation.R")
# source("bivariate_relationships.R")

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)
library(ggplot2)
library(lavaan)
library(semPlot)
library(semTools)
library(ggcorrplot)
library(lme4)
library(stargazer)

source("clust_pal.R")

# Import data ----

# Aggregated and filtered data
sem_data <- read.csv("data/plot_data_fil_agg_norm_std_outlier.csv")

# Recode fire index
sem_data<- sem_data %>%
  mutate(fire_index = case_when(
    fire_index == "Frequent" ~ 5,
    fire_index == "Occassional" ~ 4,
    fire_index == "Rare" ~ 3,
    fire_index == "Very rare" ~ 2,
    fire_index == "No fire" ~ 1
  ))
  
# Correlation matrix between variables ----
# Create a dataframe 
corr_df <- sem_data %>%
  select(sand_per_std, ocdens_std, cation_ex_cap_std, 
    total_precip_std, precip_seasonality_std, 
    mean_temp_std, temp_seasonality_std, isothermality_std,
    fire_return_mean_log_std, sp_rich_raref_std, shannon_equit_std,
    cov_height_std, cov_dbh_std, bchave_log) %>%
  rename(`Sand %` = sand_per_std,
    `Organic C %` = ocdens_std,
    `CEC` = cation_ex_cap_std,
    `MAP` = total_precip_std, 
    `MAP CoV` = precip_seasonality_std, 
    `MAT` = mean_temp_std, 
    `MAT CoV` = temp_seasonality_std,
    `Isothermality` = isothermality_std,
    `Fire return interval` = fire_return_mean_log_std, 
    `Raref. Sp. Rich.` = sp_rich_raref_std, 
    `Shannon equit.` = shannon_equit_std,
    `Tree height CoV` = cov_height_std, 
    `DBH CoV` = cov_dbh_std,
    `AGB` = bchave_log)

# Make a dataframe of labels
label_df <- data.frame(lab = names(corr_df),
  x_loc = seq(from = 1, to = length(names(corr_df)), by = 1),
  y_loc = seq(from = 0.8, to = length(names(corr_df)) - 1 + 0.8, by = 1))

# Plot
pdf(file = "img/corr_mat.pdf", width = 8, height = 8)
ggcorrplot(cor(corr_df, use = "complete.obs"), 
  type = "lower", 
  lab = TRUE,
  method = "square",
  colors = c("blue", "white", "red"),
  ggtheme = theme_classic, 
  show.legend = FALSE,
  outline.color = "black",
  digits = 2, lab_size = 4)
dev.off()

# Environmental model ----
##' Only the effects of climate and environment on biomass, 
##' No tree diversity factors
##' No latent variables

env_path_model_fit <- lmer(bchave_log ~ ocdens_std + sand_per_std + cation_ex_cap_std +
  aridity_index_std + total_precip_std + precip_seasonality_std +
  mean_temp_std + temp_seasonality_std + isothermality_std +
  fire_return_mean_log_std + (1|clust5), data = sem_data)

sink("output/env_path_model_fit.txt")
print(summary(env_path_model_fit))
print(MuMIn::r.squaredGLMM(env_path_model_fit))
sink()

# Indirect effect of diversity on biomass via structure ----

struc_model_spec <- "
# Latent vars
div     =~  1*sp_rich_raref_std + shannon_equit_std
struc   =~  1*cov_dbh_std + cov_height_std
biomass =~  1*bchave_log

# Regressions
biomass ~ c*div
biomass ~ b*struc
struc ~ a*div

# Explicitly model direct and indirect effects
biomass_div_via_struc := a*b
biomass_div_total := c + (a*b)
"

##' Lots of missing values for canopy height covariance, 
##' because many plots have no height data

struc_model_fit <- cfa(struc_model_spec, data = sem_data)

struc_model_summ <- summary(struc_model_fit, fit.measures = TRUE)

sink("output/struc_model_fit.txt")
print(struc_model_summ)
sink()

fileConn<-file("output/struc_model_coef_stargazer.txt")
writeLines(stargazer(struc_model_summ$PE, 
  summary = FALSE, rownames = FALSE, label = "struc_model_summ", digit.separate = 0), fileConn)
close(fileConn)

pdf(file = "img/struc_model.pdf", width = 12, height = 8)
semPaths(struc_model_fit ,'mod', "est", 
  layout = "circle", curvature = 1, residuals = FALSE, intercepts = FALSE, nCharNodes = 0,
  label.cex = 2, ask = FALSE, exoCov = FALSE)
dev.off()

# Dot and line plots for the slopes and errors
struc_model_regs <- struc_model_summ$PE %>% 
  filter(op %in% c("~", ":=")) %>%
  mutate(
    op = case_when(
      op == "~" & lhs == "biomass" ~ "Direct: Biomass",
      TRUE ~ "Other eff."),
    rhs = case_when(
      op == "Direct: Biomass" & rhs == "div" ~ "Diversity",
      rhs == "struc" ~ "Struct.",
      op == "Other eff." & rhs == "div" ~ "Div. -> Struct",
      rhs == "a*b" ~ "Indirect: Div. -> Struct. -> Biomass",
      rhs == "c+(a*b)" ~ "Total effect: Div. -> Biomass"))

pdf(file = "img/struc_model_slopes.pdf", width = 12, height = 4)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = struc_model_regs, aes(x = rhs, ymin = est - se, ymax = est + se),
    width = 0.2) + 
  geom_point(data = struc_model_regs, aes(x = rhs, y = est, fill = rhs),
    colour = "black", shape = 21, size = 2) +
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = "none", 
    panel.grid.major.y = element_line(colour = "#E0E0E0"))
dev.off()

# Structural SEM for each cluster ----
sem_data_clust_list <- split(sem_data, sem_data$clust5)

struc_model_fit_clust_list <- list()
struc_model_summ_clust_list <- list()
struc_model_diag_clust_list <- list()
struc_model_regs_list <- list()

for(i in 1:length(sem_data_clust_list)){
  struc_model_fit_clust_list[[i]] <- sem(struc_model_spec, data = sem_data_clust_list[[i]])

struc_model_summ_clust_list[[i]] <- summary(struc_model_fit_clust_list[[i]], fit.measures = TRUE)

struc_model_diag_clust_list[[i]] <- semPaths(struc_model_fit_clust_list[[i]] ,'mod', "est", 
  layout = "circle", curvature = 1, residuals = FALSE, intercepts = FALSE, nCharNodes = 0,
  exoCov = FALSE,
  label.cex = 2, ask = FALSE)

pdf(file = paste0("img/struc_model_clust_", sem_data_clust_list[[i]]$clust5[1], ".pdf"), width = 12, height = 4)
plot(struc_model_diag_clust_list[[i]])
dev.off()

sink(paste0("output/struc_model_fit_clust_", sem_data_clust_list[[i]]$clust5[1], ".txt"))
print(struc_model_summ_clust_list[[i]])
sink()

struc_model_regs_list[[i]] <- struc_model_summ_clust_list[[i]]$PE %>% 
  filter(op %in% c("~", ":=")) %>%
  mutate(
    op = case_when(
      op == "~" & lhs == "biomass" ~ "Direct: Biomass",
      TRUE ~ "Other eff."),
    rhs = case_when(
      op == "Direct: Biomass" & rhs == "div" ~ "Diversity",
      rhs == "struc" ~ "Struct.",
      op == "Other eff." & rhs == "div" ~ "Div. -> Struct",
      rhs == "a*b" ~ "Indirect: Div. -> Struct. -> Biomass",
      rhs == "c+(a*b)" ~ "Total effect: Div. -> Biomass"),
    model = first(sem_data_clust_list[[i]]$clust5))

pdf(file = paste0("img/struc_model_slopes_clust_", sem_data_clust_list[[i]]$clust5[1], ".pdf"), width = 12, height = 4)
print(ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = struc_model_regs_list[[i]], aes(x = rhs, ymin = est - se, ymax = est + se),
    width = 0.2) + 
  geom_point(data = struc_model_regs_list[[i]], aes(x = rhs, y = est, fill = rhs),
    colour = "black", shape = 21, size = 2) +
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = "none", 
    panel.grid.major.y = element_line(colour = "#E0E0E0")))
dev.off()
}

# Combine all into one dot and line plot
struc_model_regs_all <- do.call(rbind, struc_model_regs_list)

pdf("img/struc_model_slopes_clust_all.pdf", width = 10, height = 10)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = struc_model_regs_all, 
    aes(x = rhs, ymin = est - se, ymax = est + se, 
      colour = factor(model, levels = c("1", "2", "3", "4", "5"))),
    width = 0.4, position = position_dodge(width = 0.5)) + 
  geom_point(data = struc_model_regs_all, 
    aes(x = rhs, y = est, 
      fill = factor(model, levels = c("1", "2", "3", "4", "5"))),
    colour = "black", shape = 21, size = 2, 
    position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = clust_pal, name = "Cluster") + 
  scale_fill_manual(values = clust_pal, name = "Cluster") + 
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour = "#E0E0E0"))
dev.off()


# extract model fit statistics and save to file
struc_fit_df <- as.data.frame(sapply(struc_model_summ_clust_list, function(x){
  as.data.frame(t(data.frame(x$FIT)))
}))

struc_fit_df_clean <- struc_fit_df %>%
  mutate(stat =  row.names(.)) %>%
  select(stat = stat, 1:5) %>%
  rename_at(vars(contains('V')), funs(sub('V', 'C', .)))

sink(paste0("output/struc_model_fit_clust_stats.txt"))
  struc_fit_df_clean
sink()

struc_fit_df_clean_names <- struc_fit_df_clean$stat

struc_fit_df_clean_output <- as.data.frame(t(struc_fit_df_clean[,-1]))
names(struc_fit_df_clean_output) <- struc_fit_df_clean_names

struc_fit_df_clean_output <- struc_fit_df_clean_output %>%
  dplyr::select(npar, chisq, df, cfi, tli, logl, aic, bic, ntotal, 
    rmsea,rmsea.ci.lower, rmsea.ci.upper, srmr) %>%
  mutate(npar = round(as.numeric(npar)),
    chisq = round(as.numeric(chisq), digits = 2),
    df = round(as.numeric(df)),
    cfi = round(as.numeric(cfi), digits = 3),
    tli = round(as.numeric(tli), digits = 3),
    logl = round(as.numeric(logl), digits = 1),
    aic = round(as.numeric(aic), digits = 1),
    bic = round(as.numeric(bic), digits = 1),
    ntotal = round(as.numeric(ntotal)),
    rmsea = round(as.numeric(rmsea), digits = 2),
    rmsea.ci.lower = round(as.numeric(rmsea.ci.lower), digits = 3),
    rmsea.ci.upper = round(as.numeric(rmsea.ci.upper), digits = 3),
    srmr = round(as.numeric(srmr), digits = 3)
    )

row.names(struc_fit_df_clean_output) <- c(paste0("C", 1:5))

fileConn <- file("output/include/struc_model_fit_clust_stats.tex")
writeLines(stargazer(struc_fit_df_clean_output, 
  summary = FALSE,
  label = "struc_model_fit_clust_stats", digit.separate = 0), fileConn)
close(fileConn)


# Multiple regressions for each latent on biomass ----
# Model list
mreg_list <- list(
  mod_precip <- lm(bchave_log ~ total_precip_std + precip_seasonality_std, data = sem_data),
  mod_temp <- lm(bchave_log ~ mean_temp_std + temp_seasonality_std, data = sem_data),
  mod_div <- lm(bchave_log ~ sp_rich_raref_std + shannon_equit_std, data = sem_data),
  mod_soil <- lm(bchave_log ~ ocdens_std + sand_per_std + cation_ex_cap_std, data = sem_data),
  mod_struc <- lm(bchave_log ~ cov_dbh_std + cov_height_std, data = sem_data)
  )

names(mreg_list) <- c("precip", "temp", "div", "soil", "struc")

beta_list <- list()
comp_loading_list <- list()
comp_list <- list()
comp_mod_summ_list <- list()
for(i in 1:length(mreg_list)){
  
  # Get beta coefficients for each model term
  beta_list_mod <- list()
  for(j in 1:(length(mreg_list[[i]]$coefficients) - 1)){
    beta_list_mod[[j]] <- summary(mreg_list[[i]])$coefficients[j + 1, 1]
  }
  
  beta_list[[i]] <- beta_list_mod
  
  # Estimate composite variables using beta coeffs. as loadings
  comp_list_list <- list()
  for(x in 1:length(beta_list[[i]])){
    comp_list_list[[x]] <- as.vector(beta_list[[i]][[x]] * mreg_list[[i]]$model[x + 1])
  }
  comp_loading_list[[i]] <- comp_list_list
  
  comp_list[[i]] <- Reduce(`+`, comp_loading_list[[i]])
  
  # Run linear models with composite variables
  comp_mod_summ_list[[i]] <- summary(
    lm(mreg_list[[i]]$model$bchave_log ~ unlist(comp_list[[i]], use.names = FALSE))
    )
  
  names(comp_mod_summ_list[i]) <- names(mreg_list[i])
}

# Save model output to file
sink("output/lmm_composite_summ.txt")
struc_fit_df_clean
sink()


# Full latent variable model ----
##' Environmental and biodiversity variables
##' Latent constructs
##' Mediation 

full_latent_mod_spec <- "
# Latent vars
precip  =~ 1*total_precip_std + precip_seasonality_std
temp    =~ 1*isothermality_std + mean_temp_std + temp_seasonality_std
div     =~ 1*sp_rich_std + shannon_equit_std
soil    =~ 1*ocdens_std + sand_per_std + cation_ex_cap_std
biomass =~ 1*bchave_log

## Diversity
div ~ b*precip
div ~ soil

## Biomass
biomass ~ soil
biomass ~ c*precip
biomass ~ temp
biomass ~ a*div

# Explicitly model direct and indirect effects
biomass_precip_via_div := a*b
biomass_precip_total := c + (a*b)
"

full_latent_mod_fit <- sem(full_latent_mod_spec, 
  data = sem_data, orthogonal = TRUE)

full_latent_mod_summ <- summary(full_latent_mod_fit, fit.measures = TRUE)

sink("output/full_latent_mod_fit.txt")
print(full_latent_mod_summ)
sink()

pdf(file = "img/full_latent_mod.pdf", width = 12, height = 8)
semPaths(full_latent_mod_fit ,'mod', "est", 
  layout = "tree2", curvature = 1, 
  residuals = FALSE, intercepts = FALSE, thresholds = FALSE, nCharNodes = 0,
  exoCov = FALSE,
  label.cex = 2)
dev.off()

# Dot and line plots
full_latent_mod_regs <- full_latent_mod_summ$PE %>%
  filter(op %in% c("~", ":=")) %>%
  mutate(
    op = case_when(
      op == "~" & lhs == "biomass" ~ "Direct: biomass",
      op == "~" & lhs == "div" ~ "Direct: diversity",
      TRUE ~ "Other eff."),
    rhs = case_when(
      rhs == "a*b" ~ "Indirect: Moisture -> Div. -> Biomass",
      rhs == "c+(a*b)" ~ "Total effect: Moisture -> Biomass",
      rhs == "soil" ~ "Soil fertility",
      rhs == "div" ~ "Species diversity",
      rhs == "precip" ~ "Moisture avail.",
      rhs == "temp" ~ "Temperature",
      TRUE ~ rhs))

pdf(file = "img/full_latent_model_slopes.pdf", width = 12, height = 4)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = full_latent_mod_regs, aes(x = rhs, ymin = est - se, ymax = est + se),
    width = 0.3) + 
  geom_point(data = full_latent_mod_regs, aes(x = rhs, y = est, fill = rhs), 
    colour = "black", shape = 21, size = 2) +
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = "none", 
    panel.grid.major.y = element_line(colour = "#E0E0E0"))
dev.off()

# Full latent model for all groups
full_model_fit_clust_list <- list()
full_model_summ_clust_list <- list()
full_model_diag_clust_list <- list()
full_model_regs_list <- list()

sem_data_clust_list <- split(sem_data, sem_data$clust4)

for(i in 1:length(sem_data_clust_list)){
  full_model_fit_clust_list[[i]] <- sem(full_latent_mod_spec, data = sem_data_clust_list[[i]])
  
  full_model_summ_clust_list[[i]] <- summary(full_model_fit_clust_list[[i]], fit.measures = TRUE)
  
  full_model_diag_clust_list[[i]] <- semPaths(full_model_fit_clust_list[[i]] ,'mod', "est", 
    layout = "tree", curvature = 1, residuals = FALSE, intercepts = FALSE, nCharNodes = 0,
    exoCov = FALSE,
    label.cex = 2, ask = FALSE)
  
  pdf(file = paste0("img/full_model_clust_", sem_data_clust_list[[i]]$clust5[1], ".pdf"), width = 12, height = 4)
  plot(full_model_diag_clust_list[[i]])
  dev.off()
  
  sink(paste0("output/full_model_fit_clust_", sem_data_clust_list[[i]]$clust5[1], ".txt"))
  print(full_model_summ_clust_list[[i]])
  sink()
  
  full_model_regs_list[[i]] <- full_model_summ_clust_list[[i]]$PE %>% 
    filter(op %in% c("~", ":=")) %>%
    mutate(
      op = case_when(
        op == "~" & lhs == "biomass" ~ "Direct: biomass",
        op == "~" & lhs == "div" ~ "Direct: diversity",
        TRUE ~ "Other eff."),
      rhs = case_when(
        rhs == "a*b" ~ "Indirect: Moisture -> Div. -> Biomass",
        rhs == "c+(a*b)" ~ "Total effect: Moisture -> Biomass",
        rhs == "soil" ~ "Soil fertility",
        rhs == "div" ~ "Species diversity",
        rhs == "precip" ~ "Moisture avail.",
        rhs == "temp" ~ "Temperature",
        TRUE ~ rhs),
      model = first(sem_data_clust_list[[i]]$clust5))
  
  pdf(file = paste0("img/full_model_slopes_clust_", sem_data_clust_list[[i]]$clust5[1], ".pdf"), width = 12, height = 4)
  print(ggplot() + 
      geom_hline(yintercept = 0, linetype = 2) + 
      geom_errorbar(data = full_model_regs_list[[i]], aes(x = rhs, ymin = est - se, ymax = est + se),
        width = 0.2) + 
      geom_point(data = full_model_regs_list[[i]], aes(x = rhs, y = est, fill = rhs),
        colour = "black", shape = 21, size = 2) +
      facet_grid(op~., scales = "free_y", switch = "y") + 
      labs(x = "Factor", y = expression("Path coefficient")) + 
      coord_flip() + 
      theme_classic() + 
      theme(legend.position = "none", 
        panel.grid.major.y = element_line(colour = "#E0E0E0")))
  dev.off()
}

# Combine all into one dot and line plot
full_model_regs_all <- do.call(rbind, full_model_regs_list)

pdf("img/full_model_slopes_clust_all.pdf", width = 10, height = 10)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = full_model_regs_all, 
    aes(x = rhs, ymin = est - se, ymax = est + se, 
      colour = factor(model, levels = c("1", "2", "3", "4", "5"))),
    width = 0.4, position = position_dodge(width = 0.5)) + 
  geom_point(data = full_model_regs_all, 
    aes(x = rhs, y = est, 
      fill = factor(model, levels = c("1", "2", "3", "4", "5"))),
    colour = "black", shape = 21, size = 2, 
    position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = clust_pal, name = "Cluster") + 
  scale_fill_manual(values = clust_pal, name = "Cluster") + 
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour = "#E0E0E0"))
dev.off()





