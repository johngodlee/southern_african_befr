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
    total_precip_std, precip_seasonality_std, mean_temp_std, temp_seasonality_std,
    fire_return_mean_log_std, sp_rich_raref_std, shannon_equit_std,
    cov_height_std, cov_dbh_std, bchave_log) %>%
  rename(`Sand %` = sand_per_std,
    `Organic C %` = ocdens_std,
    `CEC` = cation_ex_cap_std,
    `MAP` = total_precip_std, 
    `MAP CoV` = precip_seasonality_std, 
    `MAT` = mean_temp_std, 
    `MAT CoV` = temp_seasonality_std,
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
  mean_temp_std + temp_seasonality_std + 
  fire_return_mean_log_std + (1|clust5), data = sem_data)

sink("output/env_path_model_fit.txt")
print(summary(env_path_model_fit))
print(MuMIn::r.squaredGLMM(env_path_model_fit))
sink()

# Indirect effect of diversity on biomass via structure ----
struc_model_spec <- "
# Latent vars
div     =~  1*sp_rich_raref_std + 1*shannon_equit_std + 1*shannon_cube_std
struc   =~  1*cov_dbh_std + 1*cov_height_std

# Regressions
bchave_log ~ c*div
bchave_log ~ b*struc
struc ~ a*div

# Explicitly model direct and indirect effects
biomass_div_via_struc := a*b
biomass_div_total := c + (a*b)
"

##' Lots of missing values for canopy height covariance, because many plots have no height data

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
  label.cex = 2, ask = FALSE)
dev.off()

# Dot and line plots for the slopes and errors
struc_model_regs <- struc_model_summ$PE %>% 
  filter(op %in% c("~", ":=")) %>%
  mutate(
    op = case_when(
      op == "~" & lhs == "bchave_log" ~ "Direct: Biomass",
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
      op == "~" & lhs == "bchave_log" ~ "Direct: Biomass",
      TRUE ~ "Other eff."),
    rhs = case_when(
      op == "Direct: Biomass" & rhs == "div" ~ "Diversity",
      rhs == "struc" ~ "Struct.",
      op == "Other eff." & rhs == "div" ~ "Div. -> Struct",
      rhs == "a*b" ~ "Indirect: Div. -> Struct. -> Biomass",
      rhs == "c+(a*b)" ~ "Total effect: Div. -> Biomass"))

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

# extract model fit statistics and save to file
struc_fit_df <- as.data.frame(sapply(struc_model_summ_clust_list, function(x){
  as.data.frame(t(data.frame(x$FIT)))
}))

struc_fit_df_clean <- struc_fit_df %>%
  mutate(stat =  rownames_to_column(struc_fit_df)[,1]) %>%
  select(stat = stat, 1:5) %>%
  rename_at(vars(contains('V')), funs(sub('V', 'C', .)))

sink(paste0("output/struc_model_fit_clust_stats.txt"))
  struc_fit_df_clean
sink()


# Model of diversity and biomass with respect to precipitation ----

sp_rich_precip_mod <- lm(bchave_log ~ shannon_cube_std * total_precip_std, data = sem_data)

summary(sp_rich_precip_mod)

# Full latent variable model ----
##' Environmental and biodiversity variables
##' Latent constructs
##' Mediation 

# Find loadings of indicator variables

## Precip
precip_biomass_lm <- lm(bchave_log ~ total_precip_std + precip_seasonality_std, data = sem_data)
total_precip_std_beta <- summary(precip_biomass_lm)$coefficients[2, 1]
precip_seasonality_std_beta <-  summary(precip_biomass_lm)$coefficients[3, 1]
precip_comp <- total_precip_std_beta * sem_data$total_precip_std + precip_seasonality_std_beta * sem_data$precip_seasonality_std
summary(lm(sem_data$bchave_log ~ precip_comp))

mod <- "
precip_comp <~  0.2033567 * total_precip_std + 0.09073649 * precip_seasonality_std
bchave_log ~ precip_comp
"
mod_sem <- sem(mod, data = sem_data, missing = "ML")
summary(mod_sem, standardize = TRUE)

## Temp
temp_biomass_lm <- lm(bchave_log ~ mean_temp_std + temp_seasonality_std, data = sem_data)
mean_temp_std_beta <- summary(temp_biomass_lm)$coefficients[2, 1]
temp_seasonality_std_beta <-  summary(temp_biomass_lm)$coefficients[3, 1]
temp_comp <- mean_temp_std_beta * sem_data$mean_temp_std + temp_seasonality_std_beta * sem_data$temp_seasonality_std
summary(lm(sem_data$bchave_log ~ temp_comp))

mod <- "
temp_comp <~  -0.1769788 * mean_temp_std + 0.1115555 * temp_seasonality_std
bchave_log ~ temp_comp
"
mod_sem <- sem(mod, data = sem_data, missing = "ML")
summary(mod_sem, standardize = TRUE, fit.measures = TRUE)

## Diversity
div_biomass_lm <- lm(bchave_log ~ sp_rich_raref_std + shannon_equit_std, data = sem_data)
sp_rich_raref_std_beta <- summary(div_biomass_lm)$coefficients[2, 1]
shannon_equit_std_beta <-  summary(div_biomass_lm)$coefficients[3, 1]
div_comp <- sp_rich_raref_std_beta * sem_data$sp_rich_raref_std + shannon_equit_std_beta * sem_data$shannon_equit_std
summary(lm(sem_data$bchave_log ~ div_comp))

mod <- "
div_comp <~  0.08263048 * sp_rich_raref_std + -0.2517915 * shannon_equit_std
bchave_log ~ div_comp
"
mod_sem <- sem(mod, data = sem_data, missing = "ML")
summary(mod_sem, standardize = TRUE, fit.measures = TRUE)

## Soil
soil_biomass_lm <- lm(bchave_log ~ ocdens_std + sand_per_std + cation_ex_cap_std, data = sem_data)
ocdens_std_beta <- summary(soil_biomass_lm)$coefficients[2, 1]
sand_per_std_beta <-  summary(soil_biomass_lm)$coefficients[3, 1]
cation_ex_cap_std_beta <-  summary(soil_biomass_lm)$coefficients[4, 1]

soil_comp <- ocdens_std_beta * sem_data$ocdens_std + 
  sand_per_std_beta * sem_data$sand_per_std + 
  cation_ex_cap_std_beta * sem_data$cation_ex_cap_std 

summary(lm(sem_data$bchave_log ~ soil_comp))

mod <- "
soil_comp <~  0.2567115 * ocdens_std + -0.059792 * sand_per_std + -0.0190841 * cation_ex_cap_std
bchave_log ~ soil_comp
"
mod_sem <- sem(mod, data = sem_data, missing = "ML")
summary(mod_sem, standardize = TRUE, fit.measures = TRUE)

# Structure
struc_biomass_lm <- lm(bchave_log ~ cov_dbh_std + cov_height_std, data = sem_data)
cov_dbh_std_beta <- summary(struc_biomass_lm)$coefficients[2, 1]
cov_height_std_beta <-  summary(struc_biomass_lm)$coefficients[3, 1]

struc_comp <- cov_dbh_std_beta * sem_data$cov_dbh_std + 
  cov_height_std_beta * sem_data$cov_height_std
summary(lm(sem_data$bchave_log ~ struc_comp))

mod <- "
struc_comp <~  0.113934 * cov_dbh_std + 0.07923529 * cov_height_std
bchave_log ~ struc_comp
"
mod_sem <- sem(mod, data = sem_data, missing = "ML")
summary(mod_sem, standardize = TRUE, fit.measures = TRUE)


# Correlation matrix of composite variables
corr_comp_df <- data.frame(precip_comp, temp_comp, 
  sem_data$fire_return_mean_log_std,
  soil_comp, struc_comp, div_comp, sem_data$bchave_log) %>%
  rename(
    `Moisture` = precip_comp, 
    `Temperature` = temp_comp, 
    `Fire` = sem_data.fire_return_mean_log_std,
    `Soil fertility` = soil_comp, 
    `Struct. div.` = struc_comp, 
    `Species div.` = div_comp, 
    `AGB` = sem_data.bchave_log)

pdf(file = "img/corr_mat_comp.pdf", width = 8, height = 8)
ggcorrplot(cor(corr_comp_df, use = "complete.obs"), 
  type = "lower", 
  lab = TRUE,
  method = "square",
  colors = c("blue", "white", "red"),
  ggtheme = theme_classic, 
  show.legend = FALSE,
  outline.color = "black",
  digits = 2, lab_size = 4)
dev.off()



full_latent_mod_spec <- "
# Latent vars
precip =~  1*total_precip_std + precip_seasonality_std + sand_per_std
fire    =~ fire_return_mean_log_std
temp =~  1*mean_temp_std + temp_seasonality_std
div =~  1*sp_rich_raref_std + shannon_equit_std
struc   =~ cov_dbh_std
soil =~  ocdens_std + sand_per_std + 1*cation_ex_cap_std

# Regressions - path analysis
## Structural complexity
struc ~ fire
struc ~ precip
struc ~ a*div

## Biomass
bchave_log ~ b*struc
bchave_log ~ soil
bchave_log ~ fire
bchave_log ~ precip
bchave_log ~ temp
bchave_log ~ c*div

# Explicitly model direct and indirect effects
biomass_div_via_struc := a*b
biomass_div_total := c + (a*b)
"

full_latent_mod_fit <- sem(full_latent_mod_spec, data = sem_data)

full_latent_mod_summ <- summary(full_latent_mod_fit, fit.measures = TRUE)

sink("output/full_latent_mod_fit.txt")
print(full_latent_mod_summ)
sink()

pdf(file = "img/full_latent_mod.pdf", width = 12, height = 8)
semPaths(full_latent_mod_fit ,'mod', "est", 
  layout = "tree", curvature = 1, 
  residuals = FALSE, intercepts = FALSE, thresholds = FALSE, nCharNodes = 0,
  label.cex = 2)
dev.off()

# Dot and line plots
full_latent_mod_regs <- full_latent_mod_summ$PE %>%
  filter(op %in% c("~", ":=")) %>%
  mutate(
    op = case_when(
      op == "~" & lhs == "biomass" ~ "Direct: biomass",
      op == "~" & lhs == "struc" ~ "Direct: struct.",
      TRUE ~ "Other eff."),
    rhs = case_when(
      rhs == "a*b" ~ "Indirect: Div. -> Struct. -> Biomass",
      rhs == "c+(a*b)" ~ "Total effect: Div. -> Biomass",
      rhs == "struc" ~ "Structural div.",
      rhs == "soil" ~ "Soil fertility",
      rhs == "fire" ~ "Fire disturbance",
      rhs == "div" ~ "Species diversity",
      rhs == "precip" ~ "Moisture avail.",
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

