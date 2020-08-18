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
# source("dat_collation.R")
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
library(psych)
library(ggnewscale)
library(maps)
library(gganimate)
library(MVN)

source("scripts/clust_defin.R")

p_format <- function(p){
  case_when(p < 0.01 ~ paste0("p <0.01"),
    p < 0.05 ~ paste0("p <0.05"),
    TRUE ~ paste0("p = ", round(p, digits = 2)))
}

corr_format <- function(id, row_num){
  paste0("\\newcommand{\\", id, "}{", "$r =$ ", corr_ci_tab$raw.r[row_num], ", ", corr_ci_tab$p[row_num], "}")
}

beta_format <- function(x, id, row_num) {
    paste0("\\newcommand{\\", id, "}{", 
      "$\\beta =$ ", round(x[row_num, "est"], 2),
      "$\\pm$", round(x[row_num, "se"], 3),", ", 
      p_format(x[row_num, "pvalue"]), "}")
}

# Import data ----

dat <- readRDS("data/plot_data_fil_agg_norm_std.rds")

# Reverse some variables ----
dat <- dat %>%
  mutate(
    precip_seas_rev_std = -1 * precip_seas_std,
    temp_stress_rev_std = -1 * temp_stress_std,
    sand_rev_std = -1 * sand_std, 
    shannon_equit_rev_std = -1 * shannon_equit_std)

# Correlation matrix between variables ----

# Create a dataframe 
corr_df <- dat %>%
  dplyr::select(soil_c_log_std, cec_std, nitrogen_log_std,
    fire_log_std, 
    precip_std, precip_seas_std, temp_stress_std, sand_std,
    n_species_raref_log_std, shannon_equit_rev_std,
    cov_height_std, cov_dbh_std, 
    n_trees_gt10_ha_log_std, agb_ha_log_std)

# Make a dataframe of labels
corr <- psych::corr.test(corr_df, alpha = 0.05, adjust = "none") 
corr_ci <- print(corr, short = FALSE)
corr_ci$vars <- row.names(corr_ci)
corr_ci$conf_x <- unlist(sapply(1:(length(corr_df)-1), function(i){
    print(c(1:(length(corr_df)-1))[i:(length(corr_df)-1)])
  })) + 1
rev_mat <- (length(corr_df)-1):1
corr_ci$conf_y <- unlist(sapply(1:(length(corr_df)-1), function(i){
    rep(i, times = rev_mat[i])
  }))
n_seq <- 2:length(corr_df)
corr_ci$n <- unlist(sapply(1:(length(corr_df)-1), function(i){corr[[2]][n_seq[i]:length(corr_df),i]}))
corr_ci$p <- round(unlist(sapply(1:(length(corr_df)-1), function(i){corr[[4]][n_seq[i]:length(corr_df),i]})), digits = 3)
corr_ci$y_var <- unlist(sapply(1:(length(corr_df)-1), function(i){rep(row.names(corr[[1]])[i], rev_mat[i])}))
corr_ci$x_var <- unlist(sapply(1:(length(corr_df)-1), function(i){row.names(corr[[1]])[n_seq[i]:length(corr_df)]}))
corr_ci$x_var <- factor(corr_ci$x_var, 
  levels = c("cec_std", "nitrogen_log_std", "fire_log_std", 
    "precip_std", "precip_seas_std", "temp_stress_std", "sand_std", "n_species_raref_log_std",
    "shannon_equit_rev_std", "cov_height_std", "cov_dbh_std", "n_trees_gt10_ha_log_std", "agb_ha_log_std"),
  labels = c("Soil CEC", "Soil N", "Fire freq.", "MAP",
  "Precip. seas.", "Temp. stress", "Sand %", "Extrap. sp. rich.", "Shannon equit",
  "Tree height CoV", "DBH CoV", "Stem density", "AGB"))
corr_ci$y_var <- factor(corr_ci$y_var, 
  levels = c("soil_c_log_std", "cec_std", "nitrogen_log_std", "fire_log_std",
    "precip_std", "precip_seas_std", "temp_stress_std", "sand_std", "n_species_raref_log_std",
    "shannon_equit_rev_std", "cov_height_std", "cov_dbh_std", "n_trees_gt10_ha_log_std"), 
  labels = c("Soil C", "Soil CEC", "Soil N", "Fire freq.", "MAP",
    "Precip. seas.", "Temp. stress", "Sand %", "Extrap. sp. rich.", "Shannon equit",
    "Tree height CoV", "DBH CoV", "Stem density"))
corr_ci$conf <- (corr_ci$raw.lower > 0) == (corr_ci$raw.upper > 0)

pdf(file = "img/corr_mat.pdf", width = 8, height = 8)
ggplot() + 
  geom_tile(data = corr_ci, 
    aes(x = x_var, y = y_var, 
      fill = raw.r), colour = "black") + 
  geom_text(data = corr_ci, 
    aes(x = x_var, y = y_var, label = raw.r),
    size = 4) + 
  geom_point(data = corr_ci[corr_ci$conf == FALSE,], 
    aes(x = x_var, y = y_var), fill = NA, colour = "black", shape = 21, size = 12) + 
  scale_fill_gradient2(name = "r", low = "blue", mid = "white", high = "red") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12,
      colour = c(rep("#D65A2D", 2), rep("#331d8b", 1), rep("#287F9C", 4), 
        rep("#468A21", 2), rep("#844099", 2), rep("black", 2))),
    axis.text.y = element_text(size = 12,
      colour = c(rep("#D65A2D", 3), rep("#331d8b", 1), rep("#287F9C", 4), 
        rep("#468A21", 2), rep("#844099", 2), "black")),
    legend.position = "none") + 
  labs(x = "", y = "") + 
  coord_equal()
dev.off()

# Correlations table
corr_ci_tab <- corr_ci %>%
  dplyr::select(x_var, y_var, raw.r, raw.lower, raw.upper, n, p)

row.names(corr_ci_tab) <- seq(1:length(corr_ci_tab$x_var))

corr_ci_tab <- corr_ci_tab %>%
  mutate(x_var = as.character(x_var),
    y_var = as.character(y_var),
    p = p_format(p))

fileConn <- file(paste0("include/corr_ci_tab.tex"))
writeLines(stargazer(corr_ci_tab, 
  summary = FALSE, label = "corr_ci_tab", digit.separate = 0, rownames = FALSE),
  fileConn)
close(fileConn)

# Correlations tex variables
fileConn <- file("include/corr_coef.tex")
writeLines(
  c(
    corr_format("ccnb", which(corr_ci_tab$y_var=="Soil N" & corr_ci_tab$x_var=="AGB")),
    corr_format("ccib", which(corr_ci_tab$y_var=="Stem density" & corr_ci_tab$x_var=="AGB")),
    corr_format("ccmb", which(corr_ci_tab$y_var=="MAP" & corr_ci_tab$x_var=="AGB")),
    corr_format("ccmcb",which(corr_ci_tab$y_var=="Precip. seas." & corr_ci_tab$x_var=="AGB")),
    corr_format("ccob", which(corr_ci_tab$y_var=="Soil C" & corr_ci_tab$x_var=="AGB")),
    corr_format("ccsb", which(corr_ci_tab$y_var=="Sand %" & corr_ci_tab$x_var=="AGB")),
    corr_format("ccms", which(corr_ci_tab$y_var=="MAP" & corr_ci_tab$x_var=="Extrap. sp. rich.")),
    corr_format("ccme", which(corr_ci_tab$y_var=="MAP" & corr_ci_tab$x_var=="Shannon equit")),
    corr_format("ccmh", which(corr_ci_tab$y_var=="MAP" & corr_ci_tab$x_var=="Tree height CoV")),
    corr_format("ccmi", which(corr_ci_tab$y_var=="MAP" & corr_ci_tab$x_var=="Stem density")),
    corr_format("ccsi", which(corr_ci_tab$y_var=="Extrap. sp. rich." & corr_ci_tab$x_var=="Stem density")),
    corr_format("ccei", which(corr_ci_tab$y_var=="Extrap. sp. rich." & corr_ci_tab$x_var=="Shannon equit")),
    corr_format("cctcb",which(corr_ci_tab$y_var=="Temp. stress" & corr_ci_tab$x_var=="AGB")),
    corr_format("ccfb", which(corr_ci_tab$y_var=="Fire freq." & corr_ci_tab$x_var=="AGB")),
    corr_format("ccfs", which(corr_ci_tab$y_var=="Fire freq." & corr_ci_tab$x_var=="Extrap. sp. rich.")),
    corr_format("ccdvi", which(corr_ci_tab$y_var=="DBH CoV" & corr_ci_tab$x_var=="Stem density")),
    corr_format("cchvi", which(corr_ci_tab$y_var=="Tree height CoV" & corr_ci_tab$x_var=="Stem density"))
    ),
  fileConn)
close(fileConn)

# Structural SEM ----

struc_model_spec <- "
# Latent vars
div     =~  n_species_raref_log_std + shannon_equit_rev_std 
struc   =~  cov_dbh_std + cov_height_std
div ~~ n_trees_gt10_ha_log_std

# Modifications

# Regressions
agb_ha_log_std ~ c*div
agb_ha_log_std ~ b*struc
struc ~ a*div
agb_ha_log_std ~ d*n_trees_gt10_ha_log_std


# Explicitly model direct and indirect effects
biomass_div_via_struc := a*b
biomass_div_total := c + (a*b) + (d)
"

##' Lots of missing values for canopy height covariance, 
##' because many plots have no height data

struc_model_fit <- sem(struc_model_spec, data = dat, estimator = "MLM")

resid(struc_model_fit, type = "cor")

struc_mod_mi <- modificationindices(struc_model_fit, sort. = TRUE)

struc_model_summ <- summary(struc_model_fit, 
  fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

struc_model_edge_df <- struc_model_summ$PE %>%
  filter(op %in% c("=~", "~", ":=")) %>%
  mutate(est = round(est, digits = 2))

sink("output/struc_model_fit.txt")
print(struc_model_summ)
sink()

pdf(file = "img/struc_mod.pdf", width = 12, height = 8)
semPaths(struc_model_fit ,'mod', "est", 
  layout = "tree2", curvature = 1, nCharNodes = 0,
  label.cex = 2, ask = FALSE, exoCov = TRUE)
dev.off()

# Dot and line plots for the slopes and errors
mod_summ_mutate_struc <- function(x){
  filter(x, op %in% c("~", ":=")) %>%
  mutate(.,
    op = case_when(
      op == "~" & lhs == "agb_ha_log_std" ~ "Direct: AGB",
      TRUE ~ "Other"),
    rhs = case_when(
      op == "Direct: AGB" & rhs == "div" ~ "Species\ndiversity",
      op == "Direct: AGB" & rhs == "struc" ~ "Structural\ndiversity",
      op == "Direct: AGB" & rhs == "n_trees_gt10_ha_log_std" ~ "Stem density",
      lhs == "struc" & op == "Other" & rhs == "div" ~ "Species diversity ->\nStructural diversity",
      lhs == "stems_ha_log_std" & op == "Other" & rhs == "div" ~ "Species diversity ->\nStem density",
      op == "Other" & rhs == "a*b" ~ "Indirect:\nSpecies diversity ->\nStructural diversity ->\nAGB",
      op == "Other" & rhs == "d*e" ~ "Indirect:\nSpecies diversity ->\nStem density ->\nAGB",
      op == "Other" & (rhs == "c+(a*b)+(d)" | rhs == "c+(a*b)+(d*e)" | rhs == "c+(a*b)")  ~ "Total effect:\nSpecies diversity ->\nAGB",
    TRUE ~ rhs))
}

struc_model_regs <- mod_summ_mutate_struc(struc_model_summ$PE)
  
pdf(file = "img/struc_model_slopes.pdf", width = 12, height = 4)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = struc_model_regs, 
    aes(x = rhs, ymin = std.all - se, ymax = std.all + se),
    width = 0.2) + 
  geom_point(data = struc_model_regs, aes(x = rhs, y = std.all, group = rhs),
    colour = "black", size = 2) +
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = "none", 
    panel.grid.major.y = element_line(colour = "#E0E0E0"))
dev.off()

# Structural SEM for each cluster ----
dat_clust_list <- split(dat, dat$clust4)

clust_mod <- function(mod, mod_name, mutate_summ){
  model_fit_clust_list <- list()
  model_summ_clust_list <- list()
  model_diag_clust_list <- list()
  model_regs_list <- list()
  for(i in 1:length(dat_clust_list)){
    model_fit_clust_list[[i]] <- sem(mod, data = dat_clust_list[[i]], estimator = "MLM")
  
  model_summ_clust_list[[i]] <- summary(model_fit_clust_list[[i]], 
    fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
  
  model_diag_clust_list[[i]] <- semPaths(model_fit_clust_list[[i]] ,'mod', "est", 
    layout = "circle", curvature = 1, residuals = FALSE, 
    intercepts = FALSE, nCharNodes = 0,
    exoCov = FALSE,
    label.cex = 2, ask = FALSE)
  
  pdf(file = paste0("img/", mod_name, "_model_clust_", 
    dat_clust_list[[i]]$clust4[1], ".pdf"), 
    width = 12, height = 4)
  plot(model_diag_clust_list[[i]])
  dev.off()
  
  sink(paste0("output/", mod_name, "_model_fit_clust_", 
    dat_clust_list[[i]]$clust4[1], ".txt"))
  print(model_summ_clust_list[[i]])
  sink()
  
  model_regs_list[[i]] <- model_summ_clust_list[[i]]$PE %>% 
    mutate_summ(.) %>%
    mutate(model = first(dat_clust_list[[i]]$clust4))
  
  pdf(file = paste0("img/", mod_name, "_model_slopes_clust_", 
    dat_clust_list[[i]]$clust4[1], ".pdf"), width = 12, height = 4)
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
      colour = factor(model, levels = c("1", "2", "3", "4", "all"), 
        labels = c(clust_names, "All"))),
    width = 0.4, position = position_dodge(width = 0.5)) + 
  geom_point(data = model_regs_all, 
    aes(x = rhs, y = est, 
      fill = factor(model, levels = c("1", "2", "3", "4", "all"), 
        labels = c(clust_names, "All"))),
    colour = "black", shape = 21, size = 2, 
    position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c(clust_pal, "black"), name = "Cluster") + 
  scale_fill_manual(values = c(clust_pal, "black"), name = "Cluster") + 
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Factor", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour = "#E0E0E0"),
    axis.text = element_text(size = 12)))
dev.off()
}

struc_model_regs$est[1] <- -struc_model_regs$est[1]

mod_slopes_all(struc_model_regs_list, struc_model_regs, 
  paste0("struc_model_slopes_all", ".pdf"))

rsquare_clust <- c(sapply(struc_model_fit_clust_list, function(x){
  lavInspect(x, "rsquare")[5]
}), lavInspect(struc_model_fit, "rsquare")[5])

sem_fit_tab <- function(mod_summ_list, mod_all, file){
mod_summ_list_all <- c(mod_summ_list, list(mod_all))

fit_df <- sapply(mod_summ_list_all, function(x){
  x$FIT
})

fit_df_clean <- fit_df %>%
  as.data.frame(.) %>%
  mutate(stat =  row.names(.)) %>%
  rename_at(vars(contains('V')), funs(sub('V', 'C', .))) %>%
  rename("All" = C5)

sink(paste0("output/", file, ".txt" ))
fit_df_clean
sink()

fit_df_clean_names <- fit_df_clean$stat

fit_df_clean_output <- as.data.frame(t(fit_df_clean[,1:5]))
clust_names_mod <- row.names(fit_df_clean_output)[1:5]

names(fit_df_clean_output) <- fit_df_clean_names

fit_df_clean_output <- fit_df_clean_output %>%
  mutate(cluster = clust_names_mod,
    npar = round(as.numeric(npar)),
    chisq = round(as.numeric(chisq), digits = 2),
    df = round(as.numeric(df)),
    cfi = round(as.numeric(cfi.scaled), digits = 3),
    tli = round(as.numeric(tli.scaled), digits = 3),
    aic = round(as.numeric(aic), digits = 1),
    ntotal = round(as.numeric(ntotal)),
    rmsea = round(as.numeric(rmsea.scaled), digits = 2),
    srmr = round(as.numeric(srmr), digits = 3),
    rsquare_agb = round(rsquare_clust, digits = 2)
    ) %>%
  dplyr::select(cluster, ntotal, chisq, df, cfi, tli, 
    rmsea, rsquare_agb)

fit_df_clean_output$cluster <- c(clust_names, "All")

fileConn <- file(paste0("include/", file, ".tex"))
writeLines(stargazer(fit_df_clean_output, 
  summary = FALSE,
  label = file, digit.separate = 0, rownames = FALSE), fileConn)
close(fileConn)
} 

sem_fit_tab(struc_model_summ_clust_list, struc_model_summ,
  file = "struc_model_fit_clust_stats")

fileConn <- file(paste0("include/path_coef_struc.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\strucrsq}{",  round(lavInspect( struc_model_fit,                 "rsquare")[5], digits = 2), "}"),
    paste0("\\newcommand{\\strucarsq}{", round(lavInspect( struc_model_fit_clust_list[[1]], "rsquare")[5], digits = 2), "}"),
    paste0("\\newcommand{\\strucbrsq}{", round(lavInspect( struc_model_fit_clust_list[[2]], "rsquare")[5], digits = 2), "}"),
    paste0("\\newcommand{\\struccrsq}{", round(lavInspect( struc_model_fit_clust_list[[3]], "rsquare")[5], digits = 2), "}"),
    paste0("\\newcommand{\\strucdrsq}{", round(lavInspect( struc_model_fit_clust_list[[4]], "rsquare")[5], digits = 2), "}"),
    beta_format(struc_model_edge_df, "strucbetadb", 5),
    beta_format(struc_model_edge_df, "strucbetadib", 9),
    beta_format(struc_model_edge_df, "strucbetaib", 8),
    beta_format(struc_model_summ_clust_list[[1]]$PE, "strucbetaasb", 5),
    beta_format(struc_model_summ_clust_list[[1]]$PE, "strucbetaahb", 6),
    beta_format(struc_model_summ_clust_list[[2]]$PE, "strucbetabsb", 5),
    beta_format(struc_model_summ_clust_list[[2]]$PE, "strucbetabhb", 6),
    beta_format(struc_model_summ_clust_list[[3]]$PE, "strucbetacsb", 5),
    beta_format(struc_model_summ_clust_list[[3]]$PE, "strucbetachb", 6),
    beta_format(struc_model_summ_clust_list[[4]]$PE, "strucbetadsb", 5),
    beta_format(struc_model_summ_clust_list[[4]]$PE, "strucbetadhb", 6)),
  fileConn)
close(fileConn)

# Structural SEM - Variation in stem density ----

# Create quantiles
min_quantile <- seq(from = 0.01, to = 0.5, by = 0.01)
max_quantile <- seq(from = 0.51, to = 1, by = 0.01)

# Create many datasets based on quantiles of stem density
dat_quant_list <- list()
for(i in 1:length(min_quantile)){
  dat_quant_list[[i]] <- subset(dat, 
    subset = (
      dat$n_trees_gt10_ha_log_std <= quantile(dat$n_trees_gt10_ha_log_std, max_quantile[[i]]) &
          dat$n_trees_gt10_ha_log_std >= quantile(dat$n_trees_gt10_ha_log_std, min_quantile[[i]])))
}

# Extract some values from each sub dataset
quant_stems_ha <- sapply(dat_quant_list, function(x){median(x$n_trees_gt10_ha)})
plot(1:length(quant_stems_ha), quant_stems_ha)

quant_plots <- sapply(dat_quant_list, nrow)
plot(1:length(quant_stems_ha), quant_plots)

quant_sp_rich_raref <- sapply(dat_quant_list, function(x){median(x$n_species_raref)})
plot(1:length(quant_stems_ha), quant_sp_rich_raref)

fileConn <- file(paste0("include/dens_stats.tex"))
writeLines(
  c(
    paste0("\\newcommand{\\subn}{", length(quant_plots), "}"),
    paste0("\\newcommand{\\subp}{", round(mean(quant_plots), digits = 0), "}")),
  fileConn)
close(fileConn)

# animated map of quantile datasets
dat_quant_list <- lapply(1:length(dat_quant_list), function(x){
  dat_quant_list[[x]]$stem_dens_round <- round(quant_stems_ha[x], digits = 0)
  dat_quant_list[[x]]$stem_dens <- quant_stems_ha[x]
  
  return(dat_quant_list[[x]])
})

# Collapse list to a dataframe
dat_quant_df <- bind_rows(dat_quant_list, .id = "column_label")

# Create vector of southern Africa ISO codes - find a way to mine the data for this
s_af <- iso.expand(c("ZAF", "COD", "NAM", "ZMB", "BWA", "ZWE", "MOZ", "MWI", "AGO", "TZA", "KEN", "COG"))

# Create map of country outlines
map_africa <- borders(database = "world", regions = s_af, fill = "grey90", colour = "black")
map_africa_fill <- borders(database = "world", regions = s_af, fill = "grey90")
map_africa_colour <- borders(database = "world", regions = s_af, colour = "black")

#pg <- ggplot(dat_quant_df) +
#  map_africa_fill + 
#  geom_point(
#    aes(x = longitude_of_centre, y = latitude_of_centre, fill = as.character(clust4)),
#    colour = "black", shape = 21, size = 2) + 
#  scale_fill_manual(values = clust_pal, name = "Cluster") + 
#  map_africa_colour +
#  ylim(-35.5, 10) + 
#  labs(title = 'Stem density: {current_frame}', x = "Longitude", y = "Latitude") + 
#  theme_classic() +
#  theme(legend.position = "right") + 
#  coord_map() + 
#  transition_manual(frames = stem_dens_round) +
#  enter_appear() + exit_disappear()

#anim_save("img/stem_dens_anim_map.gif", animation = pg, width = 500, height = 700)

# Define a model without stem density
struc_model_no_stem_dens_spec <- "
# Latent vars
div     =~  n_species_raref_log_std + shannon_equit_rev_std
struc   =~  cov_dbh_std


# Regressions
agb_ha_log_std ~ c*div
agb_ha_log_std ~ b*struc
struc ~ a*div

# Explicitly model direct and indirect effects
biomass_div_via_struc := a*b
biomass_div_total := c + (a*b)
"

# Run the model for each dataset
struc_sem_quant_list <- list()
struc_sem_quant_summ_list <- list()
struc_sem_quant_regs_list <- list()
struc_sem_quant_fit_list <- list()
for(i in 1:length(dat_quant_list)){
  struc_sem_quant_list[[i]] <- sem(struc_model_no_stem_dens_spec, 
    data = dat_quant_list[[i]], estimator = "MLM")
  struc_sem_quant_summ_list[[i]] <- summary(struc_sem_quant_list[[i]], 
    fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
  
  struc_sem_quant_regs_list[[i]] <- struc_sem_quant_summ_list[[i]]$PE %>% 
    mod_summ_mutate_struc(.) %>%
    mutate(quant = mean(min_quantile[[i]], max_quantile[[i]]),
      stems_ha = quant_stems_ha[[i]],
      n_plots = quant_plots[[i]],
      n_species_raref = quant_sp_rich_raref[[i]])
}

struc_sem_quant_fit <- lapply(struc_sem_quant_summ_list, function(x){
  data.frame(cfi = x$FIT["cfi"],
    tli = x$FIT["tli"],
    rmsea = x$FIT["rmsea"])
})

struc_sem_quant_fit_df <- do.call(rbind, struc_sem_quant_fit[sapply(struc_sem_quant_fit, nrow) != 0])

struc_sem_quant_fit_df$stems_ha <- quant_stems_ha[sapply(struc_sem_quant_fit, nrow) != 0]
struc_sem_quant_fit_df$n_species_raref <- quant_sp_rich_raref[sapply(struc_sem_quant_fit, nrow) != 0]
struc_sem_quant_fit_df$n_plots <- quant_plots[sapply(struc_sem_quant_fit, nrow) != 0]

# Extract model statistics
struc_sem_quant_regs <- do.call(rbind, struc_sem_quant_regs_list) %>%
  dplyr::select(rhs, quant, stems_ha, n_plots, n_species_raref, est, std.all) %>%
  mutate(rhs = case_when(
    rhs == "Species diversity ->\nStructural diversity " ~ "Species diversity -> Structural diversity",
    rhs == "Indirect:\nSpecies diversity ->\nStructural diversity ->\nAGB" ~ "Species diversity -> Structural diversity -> AGB",
    rhs == "Total effect:\nSpecies diversity ->\nAGB" ~ "Total effect: Species diversity -> AGB",
    rhs == "Species\ndiversity" ~ "Species diversity -> AGB",
    rhs == "Structural\ndiversity" ~ "Structural diversity -> AGB",
    TRUE ~ rhs
  ))

pdf("img/sem_struc_stems_ha.pdf", width = 10, height = 8)
ggplot(data = struc_sem_quant_regs) + 
  geom_hline(aes(yintercept = 0), linetype = 5) + 
  geom_line(aes(x = -stems_ha + 335, y = est)) +  
  geom_smooth(aes(x = -stems_ha + 335, y = est), 
    method = "loess", size = 0.5, span = 0.5, colour = "#C44620") + 
  facet_wrap(~rhs) +
  labs(x = expression("Stems" ~ ha^-1), y = "Unstandardised path coefficient") + 
  scale_x_continuous(breaks = seq(100,250, by = 25)) + 
  theme_classic() + 
  theme(axis.text = element_text(size = 12),
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 12))
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
  mod_mois <- lm(agb_ha_log ~ precip_std + precip_seas_rev_std + 
  	  temp_stress_rev_std + sand_rev_std, data = dat),
  mod_div <- lm(agb_ha_log ~ n_species_raref_log_std + shannon_equit_rev_std, data = dat),
  mod_struc <- lm(agb_ha_log ~ cov_dbh_std + cov_height_std, data = dat), 
  mod_soil <- lm(agb_ha_log ~ soil_c_log_std + nitrogen_log_std + cec_std, 
  	  data = dat),
  mod_disturbance <- lm(agb_ha_log ~ fire_log_std, data = dat)
)

names(mreg_list) <- c("mois", "div", "struc", "soil", "disturbance")

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
    lm(mreg_list[[i]]$model$agb_ha_log ~ unlist(comp_list[[i]], use.names = FALSE))
    )
  
  names(comp_mod_summ_list[i]) <- names(mreg_list[i])
}

# Save model output to file
sink("output/lmm_composite_summ.txt")
comp_mod_summ_list
sink()

# Full SEM ----
##' Environmental and biodiversity variables
##' Latent constructs
##' Mediation 

# Test for multivariate normality 
dat_multivar_norm <- dat %>%
  dplyr::select(precip_std, precip_seas_rev_std, 
    temp_stress_rev_std, sand_rev_std,
    n_species_raref_log_std, shannon_equit_rev_std, 
    soil_c_log_std, cec_std, nitrogen_log_std,
    fire, 
    cov_dbh_std, cov_height_std, 
    n_trees_gt10_ha_log_std, agb_ha_log_std) %>%
  mvn(., mvnTest = "mardia",  multivariateOutlierMethod = "adj", showNewData = TRUE)
##' Not multivariate normal at all, but does it matter with ML?

full_mod_spec <- "
# Latent vars
moisture =~ precip_std + precip_seas_std + temp_stress_rev_std + sand_rev_std
disturb  =~ fire_log_std 
soil     =~ soil_c_log_std + cec_std + nitrogen_log_std
div      =~ n_species_raref_log_std + shannon_equit_rev_std
struc    =~ cov_dbh_std + cov_height_std

## Diversity
div ~ a*moisture
div ~ j*soil
div ~ l*disturb

## Struc
struc ~ f*div

# stems_ha
n_trees_gt10_ha_log_std ~ d*moisture
n_trees_gt10_ha_log_std ~ i*soil
n_trees_gt10_ha_log_std ~ m*disturb
n_trees_gt10_ha_log_std ~ h*div

## Biomass
agb_ha_log_std ~ k*soil
agb_ha_log_std ~ c*moisture
agb_ha_log_std ~ n*disturb
agb_ha_log_std ~ b*div
agb_ha_log_std ~ g*struc
agb_ha_log_std ~ e*n_trees_gt10_ha_log_std

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
biomass_soil_total := k + (j*b) + (i*e) + (j*f*g) + (j*h*e)

biomass_disturb_via_div := l*b
biomass_disturb_via_stems := m*e
biomass_disturb_via_div_struc := l*f*g
biomass_disturb_via_div_stems := l*h*e
biomass_disturb_total := n + (l*b) + (m*e) + (l*f*g) + (l*h*e)

biomass_div_via_struc := f*g
biomass_div_via_stems := h*e
biomass_div_total := b + (f*g) + (h*e)
"

# Run model
full_mod_fit <- sem(full_mod_spec, 
  data = dat)

# Modification indices
head(modificationIndices(full_mod_fit, sort. = TRUE), n = 10)

full_mod_summ <- summary(full_mod_fit, 
  fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

full_mod_summ$PE$est[20] <- -full_mod_summ$PE$est[20]

full_model_edge_df <- full_mod_summ$PE %>%
  filter(op %in% c("=~", "~", ":=")) %>%
  mutate(est = round(est, digits = 2),
    se = round(se, digits = 3),
    p = round(pvalue, digits = 3))


fileConn <- file("include/path_coef_full.tex")
writeLines(
  c(
    beta_format(full_model_edge_df, "fmbetatotaldb", 44),
    beta_format(full_model_edge_df, "fmbetadb", 24),
    beta_format(full_model_edge_df, "fmbetadhb", 42), 
    beta_format(full_model_edge_df, "fmbetamb", 22), 
    beta_format(full_model_edge_df, "fmbetasb", 21), 
    beta_format(full_model_edge_df, "fmbetafb", 23), 
    beta_format(full_model_edge_df, "fmbetamib", 28), 
    beta_format(full_model_edge_df, "fmbetasib", 33), 
    beta_format(full_model_edge_df, "fmbetafib", 38), 
    beta_format(full_model_edge_df, "fmbetamd", 13), 
    beta_format(full_model_edge_df, "fmbetasd", 14), 
    beta_format(full_model_edge_df, "fmbetafd", 15), 
    paste0("\\newcommand{\\fmrsq}{", round(full_mod_summ$PE$est[66], digits = 2), "}"),
    paste0("\\newcommand{\\fmrmsea}{", round(full_mod_summ$FIT["rmsea"], digits = 3), "}"),
    paste0("\\newcommand{\\fmtli}{", round(full_mod_summ$FIT["tli"], digits = 3), "}"),
    paste0("\\newcommand{\\fmcfi}{", round(full_mod_summ$FIT["cfi"], digits = 3), "}"),
    paste0("\\newcommand{\\smrsq}{", round(struc_model_summ$PE$est[24] * 100, digits = 0), "}")),
  fileConn)
close(fileConn)

sink("output/full_mod_fit.txt")
print(full_mod_summ)
sink()

pdf(file = "img/full_mod.pdf", width = 12, height = 8)
semPaths(full_mod_fit ,'mod', "est", 
  layout = "tree2", curvature = 1, 
  residuals = FALSE, intercepts = FALSE, thresholds = FALSE, nCharNodes = 0,
  exoCov = FALSE,
  label.cex = 2)
dev.off()

# Dot and line plots

# Code groupings
full_mod_regs <- full_mod_summ$PE %>%
  filter(op %in% c("~", ":=")) %>%
  mutate(
  group = case_when(
    label %in% c("a", "j", "l", "f", "d", "i", "m", "h", "g", "e") ~ "Other",
    label == "k" ~ "Soil", 
    label == "c" ~ "Moisture",
    label == "n" ~ "Disturbance", 
    label == "b" ~ "Diversity", 
    grepl("_moisture_", label) ~ "Moisture",
    grepl("_soil_", label) ~ "Soil", 
    grepl("_disturb_", label) ~ "Disturbance", 
    grepl("_div_", label) ~ "Diversity"),
  effect = case_when(
    grepl("_via_div_struc", label) ~ "Indirect, via Div. via. Struc.",
    grepl("_via_div_stems", label) ~ "Indirect, via Div. via. Stem dens.",
    grepl("_via_struc", label) ~ "Indirect, via Struc.",
    grepl("_via_stems", label) ~ "Indirect, via Stem dens.",
    grepl("_via_div", label) ~ "Indirect, via Div.",
    grepl("_total", label) ~ "Total",
    lhs == "agb_ha_log_std" & rhs == "moisture" ~ "Direct",
    lhs == "agb_ha_log_std" & rhs == "soil" ~ "Direct",
    lhs == "agb_ha_log_std" & rhs == "div" ~ "Direct",
    lhs == "agb_ha_log_std" & rhs == "disturb" ~ "Direct",
    label == "g" ~ "Direct: Struc. -> AGB",
    label == "e" ~ "Direct: Stem dens. -> AGB",
    lhs == "div" & rhs == "moisture" ~ "Direct: Mois. -> Div.",
    lhs == "div" & rhs == "soil" ~ "Direct: Soil -> Div.",
    lhs == "div" & rhs == "disturb" ~ "Direct: Disturb. -> Div.",
    lhs == "struc" & rhs == "div" ~ "Direct: Div. -> Struc.",
    lhs == "n_trees_gt10_ha_log_std" & rhs == "moisture" ~ "Direct: Mois. -> Stem dens.",
    lhs == "n_trees_gt10_ha_log_std" & rhs == "soil" ~ "Direct: Soil -> Stem dens.",
    lhs == "n_trees_gt10_ha_log_std" & rhs == "disturb" ~ "Direct: Disturb. -> Stem dens.",
    lhs == "n_trees_gt10_ha_log_std" & rhs == "div" ~ "Direct: Div. -> Stem dens.")
  ) %>% 
  mutate(group = factor(group, 
      levels = c("Diversity", "Disturbance", "Moisture", "Soil", "Other"))) %>%
  filter(effect != "Indirect, via Div. via. Stem dens.", 
    !((effect == "Indirect, via Stem dens.") & (group == "Diversity")))

# Create plot
pdf(file = "img/full_model_slopes.pdf", width = 10, height = 12)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = full_mod_regs, 
    aes(x = effect, ymin = est - se, ymax = est + se),
    width = 0.3) + 
  geom_point(data = full_mod_regs, aes(x = effect, y = est, group = effect), 
    colour = "black", size = 2) +
  facet_grid(group~., scales = "free_y", switch = "y") + 
  labs(x = "", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = "none", 
    panel.grid.major.y = element_line(colour = "#E0E0E0"), 
    text = element_text(size = 14))
dev.off()

# Full SEM for each cluster ----
#clust_mod(full_mod_spec, "full", mod_summ_full)
#
## Combine all into one dot and line plot
#mod_slopes_all(full_model_regs_list, full_mod_regs, 
#  "full_model_slopes_all.pdf")

# extract model fit statistics and save to file
# sem_fit_tab(full_model_summ_clust_list, full_mod_summ, 
#   file = "full_model_fit_clust_stats")
