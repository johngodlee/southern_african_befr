# Structural Equation Models for Chapter 1 - Regional analysis of the factors affecting woody AGB and productivity
# John Godlee (johngodlee@gmail.com)
# 2019_09_20

# Notes for interpreting SEM models
##' Double headed arrows show covariances
##' Single headed arrows show causal relationships
##' Exogenous variables (i.e. independent variables) measurement error (rounded double ended arrows) of 1.00. 
##' Exogenous variables may have covariance

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
library(tidyr)

source("clust_pal.R")
source("full_best.R")

# Import data ----

# Aggregated and filtered data

sem_data <- read.csv(paste0("data/plot_data_fil_agg", ext, "_norm_std_outlier.csv"))

##' Species richness as determinant of large trees 
##' (measured as mean of 95th percentile of height) biomass

# Correlation matrix between variables ----

# Unreverse some variables
sem_data$precip_seasonality_log_std <- -sem_data$precip_seasonality_rev_log_std
sem_data$temp_seasonality_log_std <- -sem_data$temp_seasonality_rev_log_std
sem_data$sand_per_std <- -sem_data$sand_per_rev_std
sem_data$mean_temp_std <- -sem_data$mean_temp_rev_std

# Create a dataframe 
corr_df <- sem_data %>%
  select(sand_per_std, ocdens_std, cation_ex_cap_std, 
    total_precip_std, precip_seasonality_log_std,
    mean_temp_std, temp_seasonality_log_std,
    sp_rich_raref_log_std, shannon_equit_log_std,
    cov_height_std, cov_dbh_std, stems_ha_log_std, bchave_log) %>%
  rename(`Sand %` = sand_per_std,
    `Organic C %` = ocdens_std,
    `CEC` = cation_ex_cap_std,
    `MAP` = total_precip_std, 
    `MAP CoV` = precip_seasonality_log_std, 
    `MAT` = mean_temp_std, 
    `MAT CoV` = temp_seasonality_log_std,
    `Extrap. sp. rich.` = sp_rich_raref_log_std, 
    `Shannon equit.` = shannon_equit_log_std,
    `Tree height CoV` = cov_height_std, 
    `DBH CoV` = cov_dbh_std,
    `Stem density` = stems_ha_log_std,
    `AGB` = bchave_log)

# Make a dataframe of labels
label_df <- data.frame(lab = names(corr_df),
  x_loc = seq(from = 1, to = length(names(corr_df)), by = 1),
  y_loc = seq(from = 0.8, to = length(names(corr_df)) - 1 + 0.8, by = 1))

corr_ci <- print(corr.test(corr_df, alpha = 0.05, adjust = "none"), short = FALSE)
corr_ci$vars <- row.names(corr_ci)
corr_ci$conf <- (corr_ci$lower.adj > 0) == (corr_ci$upper.adj > 0)
corr_ci$conf_x <- unlist(sapply(1:12, function(i){print(c(1:12)[i:12])}))
rev_mat <- 12:1
corr_ci$conf_y <- unlist(sapply(1:12, function(i){rep(i, times = rev_mat[i])}))

# Plot
pdf(file = paste0("img/corr_mat", ext, ".pdf"), width = 8, height = 8)
ggcorrplot(cor(corr_df, use = "complete.obs"), 
  type = "lower", 
  lab = TRUE,
  method = "square",
  colors = c("blue", "white", "red"),
  ggtheme = theme_classic, 
  show.legend = FALSE,
  outline.color = "black",
  digits = 2, lab_size = 3) + 
  theme(axis.text.x = element_text(
    face = c(rep("plain", 3), "bold.italic", "plain", "bold.italic", rep("plain", 6)),
    colour = c(rep("#D65A2D", 2), rep("#287F9C", 4), rep("#468A21", 2), rep("#844099", 2), rep("black", 2))),
    axis.text.y = element_text(
      face = c("bold.italic", rep("plain", 3), "bold.italic", "plain", "bold.italic", rep("plain", 6)),
      colour = c(rep("#D65A2D", 3), rep("#287F9C", 4), rep("#468A21", 2), rep("#844099", 2), "black"))
      ) + 
  geom_point(data = filter(corr_ci, conf == FALSE), 
    aes(x = conf_x, y = conf_y), fill = NA, colour = "black", shape = 21, size = 11)
dev.off()

c("#D65A2D", "#287F9C", "#468A21", "#149CA1",  "#844099")




# Environmental model ----
##' Only the effects of climate and environment on biomass, 
##' No tree diversity factors
##' No latent variables

env_path_model_fit <- lmer(bchave_log ~ ocdens_std + sand_per_std + cation_ex_cap_std +
  aridity_index_std + total_precip_std + precip_seasonality_log_std +
  mean_temp_std + temp_seasonality_log_std + isothermality_std +
  fire_return_mean_log_std + (1|clust5), data = sem_data)

sink(paste0("output/env_path_model_fit", ext, ".txt"))
print(summary(env_path_model_fit))
print(MuMIn::r.squaredGLMM(env_path_model_fit))
sink()

# Structural SEM ----

struc_model_spec <- "
# Latent vars
div     =~  sp_rich_raref_log_std + shannon_equit_log_std
struc   =~  cov_height_std + cov_dbh_std

# Modifications


# Regressions
bchave_log_std ~ c*div
bchave_log_std ~ b*struc
struc ~ a*div
bchave_log_std ~ d*stems_ha_log_std
stems_ha_log_std ~ e*div

# Explicitly model direct and indirect effects
biomass_div_via_struc := a*b
biomass_div_via_stems := d*e
biomass_div_total := c + (a*b) + (d*e)
"

##' Lots of missing values for canopy height covariance, 
##' because many plots have no height data

struc_model_fit <- sem(struc_model_spec, data = sem_data, se = "bootstrap")

resid(struc_model_fit, type = "cor")

struc_mod_mi <- modificationindices(struc_model_fit)

struc_model_summ <- summary(struc_model_fit, 
  fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

struc_model_edge_df <- struc_model_summ$PE %>%
  filter(op %in% c("=~", "~")) %>%
  mutate(est = round(est, digits = 2))

fileConn <- file(paste0("output/include/path_coef_struc.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\pcsdsd}{", struc_model_edge_df$est[1], "}"),
    paste0("\\newcommand{\\pcsded}{", struc_model_edge_df$est[2], "}"),
    paste0("\\newcommand{\\pcshdh}{", struc_model_edge_df$est[3], "}"),
    paste0("\\newcommand{\\pcshhh}{", struc_model_edge_df$est[4], "}"),
    paste0("\\newcommand{\\pcsdb}{", struc_model_edge_df$est[5], "}"),
    paste0("\\newcommand{\\pcshb}{", struc_model_edge_df$est[6], "}"),
    paste0("\\newcommand{\\pcsdh}{", struc_model_edge_df$est[7], "}"),
    paste0("\\newcommand{\\pcsib}{", struc_model_edge_df$est[8], "}"),
    paste0("\\newcommand{\\pcsdi}{", struc_model_edge_df$est[9], "}")),
    fileConn)
close(fileConn)

sink(paste0("output/struc_model_fit", ext, ".txt"))
print(struc_model_summ)
sink()

pdf(file = paste0("img/struc_mod", ext, ".pdf"), width = 12, height = 8)
semPaths(struc_model_fit ,'mod', "est", 
  layout = "tree2", curvature = 1, nCharNodes = 0,
  label.cex = 2, ask = FALSE, exoCov = FALSE)
dev.off()

# Dot and line plots for the slopes and errors
mod_summ_mutate_struc <- function(x){
  filter(x, op %in% c("~", ":=")) %>%
  mutate(.,
    op = case_when(
      op == "~" & lhs == "bchave_log_std" ~ "Direct: AGB",
      TRUE ~ "Other eff."),
    rhs = case_when(
      op == "Direct: AGB" & rhs == "div" ~ "Diversity",
      op == "Direct: AGB" & rhs == "struc" ~ "Struct.",
      op == "Direct: AGB" & rhs == "stems_ha_log_std" ~ "Stem dens.",
      lhs == "struc" & op == "Other eff." & rhs == "div" ~ "Div. -> Struct",
      lhs == "stems_ha_log_std" & op == "Other eff." & rhs == "div" ~ "Div. -> Stem dens.",
      op == "Other eff." & rhs == "a*b" ~ "Indirect: Div. -> Struct. -> AGB",
      op == "Other eff." & rhs == "d*e" ~ "Indirect: Div. -> Stem dens -> AGB",
      op == "Other eff." & rhs == "c+(a*b)+(d*e)" ~ "Total effect: Div. -> AGB",
    TRUE ~ rhs))
}

struc_model_regs <- mod_summ_mutate_struc(struc_model_summ$PE)
  

pdf(file = paste0("img/struc_model_slopes", ext, ".pdf"), width = 12, height = 4)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = struc_model_regs, 
    aes(x = rhs, ymin = std.all - se, ymax = std.all + se),
    width = 0.2) + 
  geom_point(data = struc_model_regs, aes(x = rhs, y = std.all, fill = rhs),
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

clust_mod <- function(mod, mod_name, mutate_summ){
  model_fit_clust_list <- list()
  model_summ_clust_list <- list()
  model_diag_clust_list <- list()
  model_regs_list <- list()
  for(i in 1:length(sem_data_clust_list)){
    model_fit_clust_list[[i]] <- sem(mod, data = sem_data_clust_list[[i]])
  
  model_summ_clust_list[[i]] <- summary(model_fit_clust_list[[i]], 
    fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
  
  model_diag_clust_list[[i]] <- semPaths(model_fit_clust_list[[i]] ,'mod', "est", 
    layout = "circle", curvature = 1, residuals = FALSE, 
    intercepts = FALSE, nCharNodes = 0,
    exoCov = FALSE,
    label.cex = 2, ask = FALSE)
  
  pdf(file = paste0("img/", mod_name, "_model_clust_", 
    sem_data_clust_list[[i]]$clust5[1], ext, ".pdf"), 
    width = 12, height = 4)
  plot(model_diag_clust_list[[i]])
  dev.off()
  
  sink(paste0("output/", mod_name, "_model_fit_clust_", sem_data_clust_list[[i]]$clust5[1], ext, ".txt"))
  print(model_summ_clust_list[[i]])
  sink()
  
  model_regs_list[[i]] <- model_summ_clust_list[[i]]$PE %>% 
    mutate_summ(.) %>%
    mutate(model = first(sem_data_clust_list[[i]]$clust5))
  
  pdf(file = paste0("img/", mod_name, "_model_slopes_clust_", sem_data_clust_list[[i]]$clust5[1], ext, ".pdf"), width = 12, height = 4)
  print(ggplot() + 
    geom_hline(yintercept = 0, linetype = 2) + 
    geom_errorbar(data = model_regs_list[[i]], aes(x = rhs, ymin = est - se, ymax = est + se),
      width = 0.2) + 
    geom_point(data = model_regs_list[[i]], aes(x = rhs, y = est, fill = rhs),
      colour = "black", shape = 21, size = 2) +
    facet_grid(op~., scales = "free_y", switch = "y") + 
    labs(x = "Factor", y = expression("Path coefficient")) + 
    coord_flip() + 
    theme_classic() + 
    theme(legend.position = "none", 
      panel.grid.major.y = element_line(colour = "#E0E0E0")))
  dev.off()
  }
  assign(paste0(mod_name, "_model_regs_list"), model_regs_list, envir = .GlobalEnv)
  assign(paste0(mod_name, "_model_fit_clust_list"), model_fit_clust_list, envir = .GlobalEnv)
  assign(paste0(mod_name, "_model_summ_clust_list"), model_summ_clust_list, envir = .GlobalEnv)
  assign(paste0(mod_name, "_model_diag_clust_list"), model_diag_clust_list, envir = .GlobalEnv)
}

clust_mod(struc_model_spec, "struc", mod_summ_mutate_struc)

# Combine all into one dot and line plot

mod_slopes_all <- function(clust_mods_list, all_mod, file){

model_regs_all <- do.call(rbind, clust_mods_list)

all_mod$model <- "all"

model_regs_all <- rbind(model_regs_all, all_mod)

pdf(paste0("img/", file), width = 12, height = 10)
print(ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = model_regs_all, 
    aes(x = rhs, ymin = est - se, ymax = est + se, 
      colour = factor(model, levels = c("1", "2", "3", "4", "5", "all"))),
    width = 0.4, position = position_dodge(width = 0.5)) + 
  geom_point(data = model_regs_all, 
    aes(x = rhs, y = est, 
      fill = factor(model, levels = c("1", "2", "3", "4", "5", "all"))),
    colour = "black", shape = 21, size = 2, 
    position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c(clust_pal, "black"), name = "Cluster") + 
  scale_fill_manual(values = c(clust_pal, "black"), name = "Cluster") + 
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour = "#E0E0E0")))
dev.off()
}

mod_slopes_all(struc_model_regs_list, struc_model_regs, 
  paste0("struc_model_slopes_all", ext, ".pdf"))

sem_fit_tab <- function(mod_summ_list, mod_all, file){
mod_summ_list_all <- c(mod_summ_list, list(mod_all))
fit_df <- as.data.frame(sapply(mod_summ_list_all, function(x){
  as.data.frame(t(data.frame(x$FIT)))
}))

fit_df_clean <- fit_df %>%
  mutate(stat =  row.names(.)) %>%
  select(stat = stat, 1:6) %>%
  rename_at(vars(contains('V')), funs(sub('V', 'C', .))) %>%
  rename("All" = C6)

sink(paste0("output/", file, ".txt" ))
fit_df_clean
sink()

fit_df_clean_names <- fit_df_clean$stat

fit_df_clean_output <- as.data.frame(t(fit_df_clean[,-1]))
clust_names <- row.names(fit_df_clean_output)

names(fit_df_clean_output) <- fit_df_clean_names

fit_df_clean_output <- fit_df_clean_output %>%
  mutate(cluster = clust_names,
    npar = round(as.numeric(npar)),
    chisq = round(as.numeric(chisq), digits = 2),
    df = round(as.numeric(df)),
    cfi = round(as.numeric(cfi), digits = 3),
    tli = round(as.numeric(tli), digits = 3),
    logl = round(as.numeric(logl), digits = 1),
    aic = round(as.numeric(aic), digits = 1),
    ntotal = round(as.numeric(ntotal)),
    rmsea = round(as.numeric(rmsea), digits = 2),
    srmr = round(as.numeric(srmr), digits = 3)
    ) %>%
  dplyr::select(cluster, npar, ntotal, chisq, df, cfi, tli, logl, aic, 
    rmsea, srmr)

fileConn <- file(paste0("output/include/", file, ".tex"))
writeLines(stargazer(fit_df_clean_output, 
  summary = FALSE,
  label = file, digit.separate = 0, rownames = FALSE), fileConn)
close(fileConn)
} 

sem_fit_tab(struc_model_summ_clust_list, struc_model_summ,
  file = paste0("struc_model_fit_clust_stats", ext))

# Structural SEM - Variation in stem density ----

min_quantile <- seq(from = 0.01, to = 0.9, by = 0.001)
max_quantile <- seq(from = 0.11, to = 1, by = 0.001)

sem_data_quant_list <- list()
for(i in 1:length(min_quantile)){
  sem_data_quant_list[[i]] <- subset(sem_data, 
    subset = (
      sem_data$stems_ha_log_std <= quantile(sem_data$stems_ha_log_std, max_quantile[[i]]) &
          sem_data$stems_ha_log_std >= quantile(sem_data$stems_ha_log_std, min_quantile[[i]])))
}

quant_stems_ha <- sapply(sem_data_quant_list, function(x){median(x$stems_ha)})
quant_plots <- sapply(sem_data_quant_list, nrow)
quant_sp_rich_raref <- sapply(sem_data_quant_list, function(x){median(x$sp_rich_raref)})

struc_sem_quant_list <- list()
struc_sem_quant_summ_list <- list()
struc_sem_quant_regs_list <- list()

for(i in 1:length(sem_data_quant_list)){
  struc_sem_quant_list[[i]] <- sem(struc_model_spec, data = sem_data_quant_list[[i]])
  struc_sem_quant_summ_list[[i]] <- summary(struc_sem_quant_list[[i]], 
    fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
  
  struc_sem_quant_regs_list[[i]] <- struc_sem_quant_summ_list[[i]]$PE %>% 
    mod_summ_mutate_struc(.) %>%
    mutate(quant = mean(min_quantile[[i]], max_quantile[[i]]),
      stems_ha = quant_stems_ha[[i]],
      n_plots = quant_plots[[i]],
      sp_rich_raref = quant_sp_rich_raref[[i]])
}

struc_sem_quant_regs <- do.call(rbind, struc_sem_quant_regs_list) %>%
  select(rhs, quant, stems_ha, sp_rich_raref, n_plots, est, std.all)

pdf("img/sem_struc_stems_ha.pdf", width = 10, height = 8)
ggplot(data = struc_sem_quant_regs) + 
  geom_hline(aes(yintercept = 0), linetype = 5) + 
  geom_line(aes(x = stems_ha, y = est)) +  
  geom_smooth(aes(x = stems_ha, y = est), 
    method = "loess", size = 0.5, colour = "#C44620") + 
  facet_wrap(~rhs) +
  labs(x = expression("Stems" ~ ha^-1), y = "Path coefficient") + 
  theme_classic()
dev.off()

pdf("img/sp_rich_stems_ha.pdf", width = 10, height = 6)
ggplot(data = filter(struc_sem_quant_regs, rhs == "Diversity")) + 
  geom_point(aes(x = quant, y = sp_rich_raref)) +  
  geom_smooth(aes(x = quant, y = sp_rich_raref)) + 
  labs(x = expression("Median stems" ~ ha^-1), y = "Extrapolated species richness") + 
  theme_classic()
dev.off()

# Where is the peak of the relationship?
struc_sem_quant_regs[struc_sem_quant_regs$est == max(struc_sem_quant_regs$est),]

# Where is the minimum stem density threshold for an effect on biomass?
struc_sem_quant_regs[struc_sem_quant_regs$est == unname(sapply(
  data.frame(struc_sem_quant_regs$est), 
  function(x) x[which.min(abs(x))])),]

##' It looks like stems_ha increases the strength of the relationship
##' between diversity and biomass, but is this just because at higher 
##' stem densities you're more likely to get higher species richness?
##' I checked out the relationship between stem density and species richness and it doesn't 
##' seem to be all that strong

# Multiple regressions for each latent on biomass ----
# Model list
mreg_list <- list(
  mod_precip <- lm(bchave_log ~ total_precip_std + precip_seasonality_rev_log_std, data = sem_data),
  mod_temp <- lm(bchave_log ~ mean_temp_rev_std + temp_seasonality_rev_log_std, data = sem_data),
  mod_div <- lm(bchave_log ~ sp_rich_raref_log_std + shannon_equit_log_std, data = sem_data),
  mod_soil <- lm(bchave_log ~ ocdens_std + sand_per_rev_std + cation_ex_cap_std, data = sem_data),
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
sink(paste0("output/lmm_composite_summ", ext, ".txt"))
comp_mod_summ_list
sink()


# Full SEM ----
##' Environmental and biodiversity variables
##' Latent constructs
##' Mediation 

full_mod_spec <- "
# Latent vars
moisture =~ total_precip_std + precip_seasonality_rev_log_std + 
mean_temp_rev_std + temp_seasonality_rev_log_std
div      =~ sp_rich_raref_log_std + shannon_equit_log_std
soil     =~ sand_per_rev_std + ocdens_std + cation_ex_cap_std
struc    =~ cov_height_std + cov_dbh_std

## Diversity
div ~ a*moisture
div ~ j*soil

## Struc
#struc ~ moisture
struc ~ f*div
#struc ~ soil

# stems_ha
stems_ha_log_std ~ d*moisture
stems_ha_log_std ~ i*soil
stems_ha_log_std ~ h*div

## Biomass
bchave_log_std ~ k*soil
bchave_log_std ~ c*moisture
bchave_log_std ~ b*div
bchave_log_std ~ g*struc
bchave_log_std ~ e*stems_ha_log_std

# Indirect terms
biomass_moisture_via_div := a*b
biomass_moisture_via_stems := d*e
biomass_moisture_via_div_struc := a*f*g
biomass_moisture_via_div_stems := a*h*e
biomass_moisture_total := c + (a*b) + (d*e) + (a*f*g) + (a*h*e)

biomass_soil_via_div := j*b
biomass_soil_via_stems := i*e
biomass_soil_via_div_struc := j*f*g
biomass_soil_via_div_stems := j*h*e
biomass_soil_total := k + (j*b) + (d*e) + (a*f*g) + (a*h*e)

biomass_div_via_struc := f*g
biomass_div_via_stems := h*e
biomass_div_total := b + (f*g) + (h*e)
"

full_mod_fit <- sem(full_mod_spec, 
  data = sem_data, se = "bootstrap")

full_mod_summ <- summary(full_mod_fit, 
  fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

full_model_edge_df <- full_mod_summ$PE %>%
  filter(op %in% c("=~", "~")) %>%
  mutate(est = round(est, digits = 2))

fileConn <- file(paste0("output/include/path_coef_full.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\pcfmmp}{", full_model_edge_df$est[1], "}"),
    paste0("\\newcommand{\\pcfmmpc}{", full_model_edge_df$est[2], "}"),
    paste0("\\newcommand{\\pcfmmt}{", full_model_edge_df$est[3], "}"),
    paste0("\\newcommand{\\pcfmmtc}{", full_model_edge_df$est[4], "}"),
    paste0("\\newcommand{\\pcfdds}{", full_model_edge_df$est[5], "}"),
    paste0("\\newcommand{\\pcfdde}{", full_model_edge_df$est[6], "}"),
    paste0("\\newcommand{\\pcfsss}{", full_model_edge_df$est[7], "}"),
    paste0("\\newcommand{\\pcfsso}{", full_model_edge_df$est[8], "}"),
    paste0("\\newcommand{\\pcfssc}{", full_model_edge_df$est[9], "}"),
    paste0("\\newcommand{\\pcfhhh}{", full_model_edge_df$est[10], "}"),
    paste0("\\newcommand{\\pcfhhd}{", full_model_edge_df$est[11], "}"),
    paste0("\\newcommand{\\pcfmd}{", full_model_edge_df$est[12], "}"),
    paste0("\\newcommand{\\pcfsd}{", full_model_edge_df$est[13], "}"),
    paste0("\\newcommand{\\pcfdh}{", full_model_edge_df$est[14], "}"),
    paste0("\\newcommand{\\pcfmi}{", full_model_edge_df$est[15], "}"),
    paste0("\\newcommand{\\pcfsi}{", full_model_edge_df$est[16], "}"),
    paste0("\\newcommand{\\pcfdi}{", full_model_edge_df$est[17], "}"),
    paste0("\\newcommand{\\pcfsb}{", full_model_edge_df$est[18], "}"),
    paste0("\\newcommand{\\pcfmb}{", full_model_edge_df$est[19], "}"),
    paste0("\\newcommand{\\pcfdb}{", full_model_edge_df$est[20], "}"),
    paste0("\\newcommand{\\pcfhb}{", full_model_edge_df$est[21], "}"),
    paste0("\\newcommand{\\pcfib}{", full_model_edge_df$est[22], "}")),
  fileConn)
close(fileConn)


  sink(paste0("output/full_mod_fit", ext, ".txt"))
print(full_mod_summ)
sink()

pdf(file = paste0("img/full_mod", ext, ".pdf"), width = 12, height = 8)
semPaths(full_mod_fit ,'mod', "est", 
  layout = "tree2", curvature = 1, 
  residuals = FALSE, intercepts = FALSE, thresholds = FALSE, nCharNodes = 0,
  exoCov = FALSE,
  label.cex = 2)
dev.off()

# Dot and line plots
mod_summ_full <- function(x){
  filter(x, op %in% c("~", ":=")) %>%
  mutate(
    op = case_when(
      lhs == "bchave_log_std" & rhs == "moisture" ~ "Moisture",
      lhs == "bchave_log_std" & rhs == "soil" ~ "Soil",
      lhs == "bchave_log_std" & rhs == "div" ~ "Species\ndiversity",
      grepl("_moisture_", lhs) ~ "Moisture",
      grepl("_soil_", lhs) ~ "Soil",
      grepl("_div_", lhs) ~ "Species\ndiversity",
      TRUE ~ "Other eff."),
    rhs = case_when(
      lhs == "bchave_log_std" & rhs == "moisture" ~ "Direct",
      lhs == "bchave_log_std" & rhs == "soil" ~ "Direct",
      lhs == "bchave_log_std" & rhs == "div" ~ "Direct",
      grepl("_via_div_struc", label) ~ "Indirect, via Div. via. Struc.",
      grepl("_via_div_struc", label) ~ "Indirect, via Div. via. Struc.",
      grepl("_via_div_stems", label) ~ "Indirect, via Div. via. Stem density",
      grepl("_via_struc", label) ~ "Indirect, via Struc.",
      grepl("_via_stems", label) ~ "Indirect, via Stem density",
      grepl("_via_div", label) ~ "Indirect, via Div.",
      grepl("_total", label) ~ "Total",
      lhs == "bchave_log_std" & rhs == "struc" & op == "Other eff." ~ "Direct: Struc. -> AGB",
      lhs == "bchave_log_std" & rhs == "stems_ha_log_std" & op == "Other eff." ~ "Direct: Stem density -> AGB",
      lhs == "stems_ha_log_std" & rhs == "soil" & op == "Other eff." ~ "Direct: Soil -> Stem density",
      lhs == "stems_ha_log_std" & rhs == "moisture" & op == "Other eff." ~ "Direct: Mois. -> Stem density",
      lhs == "div" & rhs == "moisture" & op == "Other eff." ~ "Direct: Mois. -> Div.",
      lhs == "div" & rhs == "soil" & op == "Other eff." ~ "Direct: Soil -> Div.",
      lhs == "struc" & rhs == "moisture" & op == "Other eff." ~ "Direct: Mois. -> Struc.",
      lhs == "struc" & rhs == "div" & op == "Other eff." ~ "Direct: Div. -> Struc.",
      lhs == "stems_ha_log_std" & rhs == "div" & op == "Other eff." ~ "Direct: Div. -> Stem density",
      TRUE ~ rhs)) %>%
    mutate(op = factor(op, 
      levels = c('Moisture','Soil','Species\ndiversity','Other eff.')))
}

full_mod_summ$PE %>%filter(op %in% c("~", ":="))

full_mod_regs <- mod_summ_full(full_mod_summ$PE)

pdf(file = paste0("img/full_model_slopes", ext, ".pdf"), width = 12, height = 4)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = full_mod_regs, 
    aes(x = rhs, ymin = est - se, ymax = est + se),
    width = 0.3) + 
  geom_point(data = full_mod_regs, aes(x = rhs, y = est, fill = rhs), 
    colour = "black", shape = 21, size = 2) +
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Effect on AGB", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = "none", 
    panel.grid.major.y = element_line(colour = "#E0E0E0"))
dev.off()

# Full SEM for each cluster ----
clust_mod(full_mod_spec, "full", mod_summ_full)

warnings()

# Combine all into one dot and line plot
mod_slopes_all(full_model_regs_list, full_mod_regs, 
  paste0("full_model_slopes_all", ext, ".pdf"))

# extract model fit statistics and save to file
sem_fit_tab(full_model_summ_clust_list, full_mod_summ, 
  file = paste0("full_model_fit_clust_stats", ext))


# Chi squared test of full model vs. structural model ----
lavTestLRT(full_mod_fit, struc_model_fit, 
  method = "default", A.method = "delta",
  scaled.shifted = TRUE,
  H1 = TRUE, type = "Chisq", model.names = NULL)

##' It might make it impossible to compare models if they contain different
##' sets of observed variables, whether they are nested or not. It seems 
##' this test is made more for testing different correlation structures and paths.

# Precipitation and soil as moderators ----
##' Uses latent variables as interaction terms

# CFA to get latent constructs
precip_cfa_spec <- "
moisture =~ total_precip_std + precip_seasonality_rev_log_std + 
mean_temp_rev_std + temp_seasonality_rev_log_std
div      =~ sp_rich_raref_log_std + shannon_equit_log_std
soil     =~ sand_per_std + ocdens_std + cation_ex_cap_std
struc    =~ cov_dbh_std + cov_height_std
"

precip_cfa_fit <- cfa(precip_cfa_spec, sem_data, missing = "ml.x")

# 2. extract the predicted values of the cfa and add them to the dataframe
sem_data <- data.frame(sem_data, predict(precip_cfa_fit))

# Create variable with interaction of precip and div
sem_data <- sem_data %>%
  mutate(moisture_div_int = moisture * div,
    soil_div_int = soil * div)

# Regression with predefined interactions
int_spec <- "
bchave_log_std ~ div + moisture_div_int + soil_div_int
"

precip_int_fit <- sem(int_spec, sem_data)

int_mod_summ <- summary(precip_int_fit, 
  rsquare = TRUE,  fit.measures = TRUE, standardized = TRUE)

semPaths(precip_int_fit,'mod', "est", 
  layout = "tree2", curvature = 1, 
  residuals = FALSE, intercepts = FALSE, thresholds = FALSE, nCharNodes = 0,
  exoCov = FALSE,
  label.cex = 2)

sink(paste0("output/int_mod_fit", ext, ".txt"))
print(int_mod_summ)
sink()

