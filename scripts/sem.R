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
library(beepr)
library(ggnewscale)
library(maps)
library(gganimate)
library(MVN)

source("scripts/clust_defin.R")

# Import data ----

dat <- readRDS("data/plot_data_fil_agg_norm_std.rds")


# Reverse some variables ----
dat <- dat %>%
  mutate(
    precip_seas_rev_std = -1 * precip_seas_std,
    temp_seas_rev_std = -1 * temp_seas_std,
    temp_stress_rev_std = -1 * temp_stress_std,
    sand_rev_std = -1 * sand_std,
    temp_rev_std = -1 * temp_std,
    shannon_equit_rev_std = -1 * shannon_equit_std)


# Correlation matrix between variables ----

# Create a dataframe 
corr_df <- dat %>%
  dplyr::select(sand_std, soil_c_log_std, cec_std, 
    fire_buffer_log_std, herbivory_std,
    precip_std, precip_seas_std,
    temp_std, temp_stress_std,
    n_species_raref_log_std, shannon_equit_std,
    cov_height_std, cov_dbh_std, n_trees_gt10_ha_log_std, agb_ha_log_std)

# Make a dataframe of labels
label_df <- data.frame(lab = names(corr_df),
  x_loc = seq(from = 1, to = length(names(corr_df)), by = 1),
  y_loc = seq(from = 0.8, to = length(names(corr_df)) - 1 + 0.8, by = 1))

corr <- psych::corr.test(corr_df, alpha = 0.05, adjust = "none")
corr_ci <- print(corr, short = FALSE)
corr_ci$vars <- row.names(corr_ci)
corr_ci$conf <- (corr_ci$lower.adj > 0) == (corr_ci$upper.adj > 0)
corr_ci$conf_x <- unlist(sapply(1:14, function(i){print(c(1:14)[i:14])}))
rev_mat <- 14:1
corr_ci$conf_y <- unlist(sapply(1:14, function(i){rep(i, times = rev_mat[i])}))
n_seq <- 2:15
corr_ci$n <- unlist(sapply(1:14, function(i){corr[[2]][n_seq[i]:15,i]}))
corr_ci$p <- round(unlist(sapply(1:14, function(i){corr[[4]][n_seq[i]:15,i]})), digits = 3)
corr_ci$x_var <- unlist(sapply(1:14, function(i){rep(row.names(corr[[1]])[i], rev_mat[i])}))
corr_ci$y_var <- unlist(sapply(1:14, function(i){row.names(corr[[1]])[n_seq[i]:15]}))

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
  digits = 2, lab_size = 3) + 
  scale_y_discrete(labels = c(
    "sand_std" = expression(bolditalic(underline("Sand %"))),
    "soil_c_log_std" = "Org. C (ppt)",
    "cec_std" = "CEC", 
    "fire_buffer_log_std" = "Fire freq.",
    "herbivory_std" = "Herbivore biomass",
    "precip_std" = "MAP",
    "precip_seas_std" = expression(bolditalic(underline("PS"))), 
    "temp_std" = expression(bolditalic(underline("MAT"))),
    "temp_stress_std" = expression(bolditalic(underline("TS"))),
    "n_species_raref_log_std" = "Extrap. sp. rich.",
    "shannon_equit_std" = "Shannon equit.",
    "cov_height_std" = "Tree height CoV",
    "cov_dbh_std" = "CBH CoV",
    "n_trees_gt10_ha_log_std" = "Stem density")) + 
  scale_x_discrete(labels = c(
    "soil_c_log_std" = "Org. C (ppt)",
    "cec_std" = "CEC", 
    "fire_buffer_log_std" = "Fire freq.",
    "herbivory_std" = "Herbivore biomass",
    "precip_std" = "MAP",
    "precip_seas_std" = expression(bolditalic(underline("PS"))), 
    "temp_std" = expression(bolditalic(underline("MAT"))),
    "temp_stress_std" = expression(bolditalic(underline("TS"))),
    "n_species_raref_log_std" = "Extrap. sp. rich.",
    "shannon_equit_std" = "Shannon equit.",
    "cov_height_std" = "Tree height CV",
    "cov_dbh_std" = "DBH CV",
    "n_trees_gt10_ha_log_std" = "Stem density",
    "agb_ha_log_std" = "AGB")) + 
  theme(axis.text.x = element_text(
    face = c(rep("plain", 3), "bold.italic", "plain", "bold.italic", rep("plain", 6)),
    colour = c(rep("#D65A2D", 2), rep("#331d8b", 2), rep("#287F9C", 4), rep("#468A21", 2), rep("#844099", 2), rep("black", 2))),

    axis.text.y = element_text(
      face = c("bold.italic", rep("plain", 3), "bold.italic", "plain", "bold.italic", rep("plain", 6)),
      colour = c(rep("#D65A2D", 3), rep("#331d8b", 2), rep("#287F9C", 4), rep("#468A21", 2), rep("#844099", 2), "black"))
      ) + 
  geom_point(data = filter(corr_ci, conf == FALSE), 
    aes(x = conf_x, y = conf_y), fill = NA, colour = "black", shape = 21, size = 11)
dev.off()

# Correlations table
corr_ci_tab <- corr_ci %>%
  dplyr::select(x_var, y_var, raw.r, raw.lower, raw.upper, n, p)

row.names(corr_ci_tab) <- seq(1:length(corr_ci_tab$x_var))

p_format <- function(p){
  case_when(p < 0.01 ~ paste0("p <0.01"),
    p < 0.05 ~ paste0("p <0.05"),
    TRUE ~ paste0("p = ", round(p, digits = 2)))
}

corr_format <- function(id, row_num){
  paste0("\\newcommand{\\", id, "}{", "$r =$ ", corr_ci_tab$raw.r[row_num], ", ", corr_ci_tab$p[row_num], "}")
}

corr_ci_tab <- corr_ci_tab %>%
  mutate(x_var = case_when(
    x_var == "sand_std" ~ "Sand %" ,
    x_var == "soil_c_log_std" ~ "Org. C (ppt)",
    x_var == "cec_std" ~ "CEC",
    x_var == "fire_buffer_log_std" ~ "Fire freq.",
    x_var == "herbivory_std" ~ "Herbivore biomass",
    x_var == "precip_std" ~ "MAP",
    x_var == "precip_seas_std" ~ "PS",
    x_var == "temp_std" ~ "MAT",
    x_var == "temp_stress_std" ~ "TS",
    x_var == "n_species_raref_log_std" ~ "Sp. rich.",
    x_var == "shannon_equit_std" ~ "Shannon equit.",
    x_var == "cov_height_std" ~ "Tree height CV",
    x_var == "cov_dbh_std" ~ "DBH CV",
    x_var == "n_trees_gt10_ha_log_std" ~ "Stems ha",
    TRUE ~ x_var
  ),
    y_var = case_when(
    y_var == "sand_std" ~ "Sand %" ,
    y_var == "soil_c_log_std" ~ "Org. C (ppt)",
    y_var == "cec_std" ~ "CEC",
    x_var == "fire_buffer_log_std" ~ "Fire freq.",
    x_var == "herbivory_std" ~ "Herbivore biomass",
    y_var == "precip_std" ~ "MAP",
    y_var == "precip_seas_std" ~ "PS",
    y_var == "temp_std" ~ "MAT",
    y_var == "temp_stress_std" ~ "TS",
    y_var == "n_species_raref_log_std" ~ "Sp. rich.",
    y_var == "shannon_equit_std" ~ "Shannon equit.",
    y_var == "cov_height_std" ~ "Tree height CV",
    y_var == "cov_dbh_std" ~ "DBH CV",
    y_var == "n_trees_gt10_ha_log_std" ~ "Stems ha",
    y_var == "agb_ha_log_std" ~ "AGB",
    TRUE ~ y_var
    ),
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
    corr_format("ccib", corr_ci_tab[corr_ci_tab$x_var=="Stems ha" & corr_ci_tab$y_var=="AGB", "raw.r"]),
    corr_format("ccmb", corr_ci_tab[corr_ci_tab$x_var=="MAP" & corr_ci_tab$y_var=="AGB", "raw.r"]),
    corr_format("ccmcb", corr_ci_tab[corr_ci_tab$x_var=="PS" & corr_ci_tab$y_var=="AGB", "raw.r"]),
    corr_format("ccob", corr_ci_tab[corr_ci_tab$x_var=="Org. C (ppt)" & corr_ci_tab$y_var=="AGB", "raw.r"]),
    corr_format("ccsb", corr_ci_tab[corr_ci_tab$x_var=="Sand %" & corr_ci_tab$y_var=="AGB", "raw.r"]),
    corr_format("ccms", corr_ci_tab[corr_ci_tab$x_var=="MAP" & corr_ci_tab$y_var=="Sp. rich.", "raw.r"]),
    corr_format("ccme", corr_ci_tab[corr_ci_tab$x_var=="MAP" & corr_ci_tab$y_var=="Shannon equit.", "raw.r"]),
    corr_format("ccmh", corr_ci_tab[corr_ci_tab$x_var=="MAP" & corr_ci_tab$y_var=="Tree height CV", "raw.r"]),
    corr_format("ccmi", corr_ci_tab[corr_ci_tab$x_var=="MAP" & corr_ci_tab$y_var=="Stems ha", "raw.r"]),
    corr_format("ccsi", corr_ci_tab[corr_ci_tab$x_var=="Sp. rich." & corr_ci_tab$y_var=="Stems ha", "raw.r"]),
    corr_format("ccei", corr_ci_tab[corr_ci_tab$x_var=="Sp. rich." & corr_ci_tab$y_var=="Shannon equit.", "raw.r"]),
    corr_format("cctb", corr_ci_tab[corr_ci_tab$x_var=="MAT" & corr_ci_tab$y_var=="AGB", "raw.r"]),
    corr_format("cctcb", corr_ci_tab[corr_ci_tab$x_var=="TS" & corr_ci_tab$y_var=="AGB", "raw.r"])
    ),
  fileConn)
close(fileConn)

# Structural SEM ----

struc_model_spec <- "
# Latent vars
div     =~  n_species_raref_log_std + shannon_equit_std 
struc   =~  cov_dbh_std + cov_height_std

# Modifications

# Regressions
agb_ha_log_std ~ c*div
agb_ha_log_std ~ b*struc
struc ~ a*div
agb_ha_log_std ~ d*n_trees_gt10_ha_log_std
div ~~ n_trees_gt10_ha_log_std


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
      TRUE ~ "Other effects"),
    rhs = case_when(
      op == "Direct: AGB" & rhs == "div" ~ "Species div.",
      op == "Direct: AGB" & rhs == "struc" ~ "Struct. div.",
      op == "Direct: AGB" & rhs == "stems_ha_log_std" ~ "Stem dens.",
      lhs == "struc" & op == "Other effects" & rhs == "div" ~ "Species div. -> Struct. div.",
      lhs == "stems_ha_log_std" & op == "Other effects" & rhs == "div" ~ "Species div. -> Stem dens.",
      op == "Other effects" & rhs == "a*b" ~ "Indirect: Species div. -> Struct. div. -> AGB",
      op == "Other effects" & rhs == "d*e" ~ "Indirect: Species div. -> Stem dens. -> AGB",
      op == "Other effects" & (rhs == "c+(a*b)+(d)" | rhs == "c+(a*b)+(d*e)" | rhs == "c+(a*b)")  ~ "Total effect: Species div. -> AGB",
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
  theme(panel.grid.major.y = element_line(colour = "#E0E0E0")))
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
    paste0("\\newcommand{\\pcsdsd}{", struc_model_edge_df$est[1], "}"),
    paste0("\\newcommand{\\pcsded}{", struc_model_edge_df$est[2], "}"),
    paste0("\\newcommand{\\pcshdh}{", struc_model_edge_df$est[3], "}"),
    paste0("\\newcommand{\\pcshhh}{", struc_model_edge_df$est[4], "}"),
    paste0("\\newcommand{\\pcsdb}{", -struc_model_edge_df$est[5], "}"),
    paste0("\\newcommand{\\pcshb}{", struc_model_edge_df$est[6], "}"),
    paste0("\\newcommand{\\pcsdh}{", struc_model_edge_df$est[7], "}"),
    paste0("\\newcommand{\\pcsib}{", struc_model_edge_df$est[8], "}"),
    paste0("\\newcommand{\\pcsdi}{", struc_model_edge_df$est[9], "}"),
    paste0("\\newcommand{\\strucrsq}{", round(lavInspect(struc_model_fit, "rsquare")[5], digits = 2), "}"),
    paste0("\\newcommand{\\strucbrsq}{", round(lavInspect( struc_model_fit_clust_list[[2]], "rsquare")[5], digits = 2), "}"),
    paste0("\\newcommand{\\strucarsq}{", round(lavInspect( struc_model_fit_clust_list[[1]], "rsquare")[5], digits = 2), "}"),
    paste0("\\newcommand{\\strucsib}{$\\beta =$ ",  
      struc_model_edge_df$est[8], 
      "$\\pm$", 
      round(struc_model_edge_df$se[8], digits = 3), 
      ", ", 
      p_format(struc_model_edge_df$p[8]), 
      "}"),
    paste0("\\newcommand{\\strucdb}{$\\beta =$ ",  
      -struc_model_edge_df$est[5], 
      "$\\pm$", 
      round(struc_model_edge_df$se[5], digits = 3), 
      ", ", 
      p_format(struc_model_edge_df$p[5]), 
      "}"),
    paste0("\\newcommand{\\strucdsb}{$\\beta =$ ",  
      struc_model_edge_df$est[9], 
      "$\\pm$", 
      round(struc_model_edge_df$se[9], digits = 3), 
      ", ", 
      p_format(struc_model_edge_df$p[9]), 
      "}"),
    paste0(
      "\\newcommand{\\strucbsb}{$\\beta =$ ", 
      round(struc_model_summ_clust_list[[2]]$PE$est[5], digits = 2), 
      "$\\pm$", 
      round(struc_model_summ_clust_list[[2]]$PE$se[5], digits = 3), 
      ", ", 
      p_format(struc_model_summ_clust_list[[2]]$PE$pvalue[5]), 
      "}"),
    paste0(
      "\\newcommand{\\strucbhb}{$\\beta =$ ",
      round(struc_model_summ_clust_list[[2]]$PE$est[6], digits = 2),
      "$\\pm$", 
      round(struc_model_summ_clust_list[[2]]$PE$se[6], digits = 3), 
      ", ", 
      p_format(struc_model_summ_clust_list[[2]]$PE$pvalue[6]), 
      "}"),
    paste0(
      "\\newcommand{\\struccsb}{$\\beta =$ ", 
      round(struc_model_summ_clust_list[[3]]$PE$est[5], digits = 2), 
      "$\\pm$", 
      round(struc_model_summ_clust_list[[3]]$PE$se[5], digits = 3), 
      ", ", 
      p_format(struc_model_summ_clust_list[[3]]$PE$pvalue[5]), 
      "}"),
    paste0(
      "\\newcommand{\\struacsb}{$\\beta =$ ", 
      round(struc_model_summ_clust_list[[1]]$PE$est[5], digits = 2), 
      "$\\pm$", 
      round(struc_model_summ_clust_list[[1]]$PE$se[5], digits = 3), 
      ", ", 
      p_format(struc_model_summ_clust_list[[1]]$PE$pvalue[5]), 
      "}")),
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

pg <- ggplot(dat_quant_df) +
  map_africa_fill + 
  geom_point(
    aes(x = longitude_of_centre, y = latitude_of_centre, fill = as.character(clust4)),
    colour = "black", shape = 21, size = 2) + 
  scale_fill_manual(values = clust_pal, name = "Cluster") + 
  map_africa_colour +
  ylim(-35.5, 10) + 
  labs(title = 'Stem density: {current_frame}', x = "Longitude", y = "Latitude") + 
  theme_classic() +
  theme(legend.position = "right") + 
  coord_map() + 
  transition_manual(frames = stem_dens_round) +
  enter_appear() + exit_disappear()

anim_save("img/stem_dens_anim_map.gif", animation = pg, width = 500, height = 700)

# Define a model without stem density
struc_model_no_stem_dens_spec <- "
# Latent vars
div     =~  n_species_raref_log_std + shannon_equit_std
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
  select(rhs, quant, stems_ha, n_plots, n_species_raref, est, std.all) %>%
  mutate(rhs = case_when(
    rhs == "Div. -> Struct" ~ "Species div -> Struct. div",
    rhs == "Indirect: Div. -> Struct. -> AGB" ~ "Indirect: Species div -> Struct. div -> AGB",
    rhs == "Total effect: Div. -> AGB" ~ "Total effect: Species div -> AGB",
    rhs == "Diversity" ~ "Species div. -> AGB",
    rhs == "Struct." ~ "Struct. div -> AGB",
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
  mod_mois <- lm(agb_ha_log ~ precip_std + precip_seas_rev_std + 
    temp_rev_std + temp_stress_rev_std, data = dat),
  mod_div <- lm(agb_ha_log ~ n_species_raref_log_std + shannon_equit_std, data = dat),
  mod_soil <- lm(agb_ha_log ~ soil_c_log_std + sand_rev_std + cec_std, data = dat)
)

names(mreg_list) <- c("mois", "div", "soil")

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
    temp_rev_std, temp_stress_rev_std,
    n_species_raref_log_std, shannon_equit_std,
    sand_rev_std, soil_c_log_std, cec_std,
    fire, herbivory,
    cov_dbh_std, cov_height_std, n_trees_gt10_ha_log_std, agb_ha_log_std) %>%
  mvn(., mvnTest = "mardia",  multivariateOutlierMethod = "adj", showNewData = TRUE)
##' Not multivariate normal at all, but does it matter with ML?

full_mod_spec <- "
# Latent vars
moisture =~ precip_std + precip_seas_rev_std + 
temp_rev_std + temp_stress_rev_std
disturb  =~ fire_buffer_log_std + herbivory_std
soil     =~ sand_rev_std + soil_c_log_std + cec_std
div      =~ n_species_raref_log_std + shannon_equit_std
struc    =~ cov_dbh_std + cov_height_std

## Diversity
div ~ a*moisture
div ~ j*soil
div ~ l*disturb

## Struc
#struc ~ moisture
struc ~ f*div
#struc ~ soil

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
biomass_soil_total := k + (j*b) + (d*e) + (a*f*g) + (a*h*e)

biomass_disturb_via_div := l*b
biomass_disturb_via_stems := m*e
biomass_disturb_via_div_struc := l*f*g
biomass_disturb_via_div_stems := l*h*e
biomass_disturb_total := n + (l*b) + (d*e) + (a*f*g) + (a*h*e)

biomass_div_via_struc := f*g
biomass_div_via_stems := h*e
biomass_div_total := b + (f*g) + (h*e)

# Modifications
#mean_temp_rev_std ~~ temp_stress_rev_log_std 
#sp_rich_raref_log_std ~~ stems_ha_log_std 
#total_precip_std ~~ mean_temp_rev_std 
#total_precip_std ~~ sand_per_rev_std 

"

# Run model
full_mod_fit <- sem(full_mod_spec, 
  data = dat,  estimator = "MLM")

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
    paste0("\\newcommand{\\rgmbd}{", "$\\beta =$ ", full_model_edge_df$est[23], "$\\pm$", full_model_edge_df$se[23],", ", p_format(full_model_edge_df$p[23]), "}"),
    paste0("\\newcommand{\\rgsbd}{",  "$\\beta =$ ", full_model_edge_df$est[28], "$\\pm$", full_model_edge_df$se[28],", ", p_format(full_model_edge_df$p[28]), "}"),
    paste0("\\newcommand{\\rgid}{",  "$\\beta =$ ", full_model_edge_df$est[17], "$\\pm$", full_model_edge_df$se[17],", ", p_format(full_model_edge_df$p[17]), "}"),
    paste0("\\newcommand{\\rgbd}{",  "$\\beta =$ ", full_model_edge_df$est[35], "$\\pm$", full_model_edge_df$se[35],", ", p_format(full_model_edge_df$p[35]), "}"),
    paste0("\\newcommand{\\rghb}{",  "$\\beta =$ ", full_model_edge_df$est[21], "$\\pm$", full_model_edge_df$se[21],", ", p_format(full_model_edge_df$p[21]), "}"),
    paste0("\\newcommand{\\rgbhd}{",  "$\\beta =$ ", full_model_edge_df$est[33], "$\\pm$", full_model_edge_df$se[33],", ", p_format(full_model_edge_df$p[33]), "}"),
    paste0("\\newcommand{\\rgbsd}{",  "$\\beta =$ ", full_model_edge_df$est[20], "$\\pm$", full_model_edge_df$se[20],", ", p_format(full_model_edge_df$p[20]), "}"),
    paste0("\\newcommand{\\fmrsq}{", round(full_mod_summ$PE$est[66], digits = 2), "}"),
    paste0("\\newcommand{\\fmrmsea}{", round(full_mod_summ$FIT["rmsea"], digits = 3), "}"),
    paste0("\\newcommand{\\fmtli}{", "0.905", "}"), #round(full_mod_summ$FIT["tli"], digits = 3), "}"),
    paste0("\\newcommand{\\fmcfi}{", "0.924", "}"), #round(full_mod_summ$FIT["cfi"], digits = 3), "}"),
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
    paste0("\\newcommand{\\pcfib}{", full_model_edge_df$est[22], "}"),
    paste0("\\newcommand{\\srsq}{", round(struc_model_summ$PE$est[24] * 100, digits = 0), "}")),
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
mod_summ_full <- function(x){
  filter(x, op %in% c("~", ":=")) %>%
  mutate(
    op = case_when(
      lhs == "agb_ha_log_std" & rhs == "moisture" ~ "Moisture",
      lhs == "agb_ha_log_std" & rhs == "soil" ~ "Soil",
      lhs == "agb_ha_log_std" & rhs == "div" ~ "Species\ndiversity",
      grepl("moisture", lhs) ~ "Moisture",
      grepl("soil", lhs) ~ "Soil",
      grepl("div", lhs) ~ "Species\ndiversity",
      TRUE ~ "Other effects"),
    rhs = case_when(
      lhs == "agb_ha_log_std" & rhs == "moisture" ~ "Direct",
      lhs == "agb_ha_log_std" & rhs == "soil" ~ "Direct",
      lhs == "agb_ha_log_std" & rhs == "div" ~ "Direct",
      grepl("_via_div_struc", label) ~ "Indirect, via Div. via. Struc.",
      grepl("_via_div_struc", label) ~ "Indirect, via Div. via. Struc.",
      grepl("_via_div_stems", label) ~ "Indirect, via Div. via. Stem density",
      grepl("_via_struc", label) ~ "Indirect, via Struc.",
      grepl("_via_stems", label) ~ "Indirect, via Stem density",
      grepl("_via_div", label) ~ "Indirect, via Div.",
      grepl("_total", label) ~ "Total",
      lhs == "agb_ha_log_std" & rhs == "struc" & op == "Other effects" ~ "Direct: Struc. -> AGB",
      lhs == "agb_ha_log_std" & rhs == "n_trees_gt10_ha_log_std" & op == "Other effects" ~ "Direct: Stem density -> AGB",
      lhs == "n_trees_gt10_ha_log_std" & rhs == "soil" & op == "Other effects" ~ "Direct: Soil -> Stem density",
      lhs == "n_trees_gt10_ha_log_std" & rhs == "moisture" & op == "Other effects" ~ "Direct: Mois. -> Stem density",
      lhs == "div" & rhs == "moisture" & op == "Other effects" ~ "Direct: Mois. -> Div.",
      lhs == "div" & rhs == "soil" & op == "Other effects" ~ "Direct: Soil -> Div.",
      lhs == "struc" & rhs == "moisture" & op == "Other effects" ~ "Direct: Mois. -> Struc.",
      lhs == "struc" & rhs == "div" & op == "Other effects" ~ "Direct: Div. -> Struc.",
      lhs == "n_trees_gt10_ha_log_std" & rhs == "div" & op == "Other effects" ~ "Direct: Div. -> Stem density",
      TRUE ~ rhs)) %>%
    mutate(op = factor(op, 
      levels = c('Moisture','Soil','Species\ndiversity','Other effects')))
}

full_mod_summ$PE %>%filter(op %in% c("~", ":="))

full_mod_regs <- mod_summ_full(full_mod_summ$PE)

pdf(file = "img/full_model_slopes.pdf", width = 12, height = 8)
ggplot() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_errorbar(data = full_mod_regs, 
    aes(x = rhs, ymin = est - se, ymax = est + se),
    width = 0.3) + 
  geom_point(data = full_mod_regs, aes(x = rhs, y = est, group = rhs), 
    colour = "black", size = 2) +
  facet_grid(op~., scales = "free_y", switch = "y") + 
  labs(x = "Effect on AGB", y = expression("Path coefficient")) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = "none", 
    panel.grid.major.y = element_line(colour = "#E0E0E0"), 
    text = element_text(size = 14))
dev.off()

# Full SEM for each cluster ----
clust_mod(full_mod_spec, "full", mod_summ_full)

# Combine all into one dot and line plot
mod_slopes_all(full_model_regs_list, full_mod_regs, 
  "full_model_slopes_all.pdf")

# extract model fit statistics and save to file
#sem_fit_tab(full_model_summ_clust_list, full_mod_summ, 
#  file = "full_model_fit_clust_stats")

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

# Regression with predefined interactions
int_df <- data.frame(agb_ha_log_std = dat$agb_ha_log_std, 
  n_species_raref_log_std = comp_list[[2]]$n_species_raref_log_std, 
  precip_std = comp_list[[1]]$precip_std, 
  soil_c_log_std = comp_list[[3]]$soil_c_log_std) %>%
  mutate_all(.funs = list(std = ~(scale(.) %>% as.vector)))

mois_div_int_mod <- lm(agb_ha_log_std_std ~ n_species_raref_log_std_std + precip_std_std + 
    n_species_raref_log_std_std*precip_std_std , data = int_df)

z1 <- seq(min(int_df$n_species_raref_log_std_std), max(int_df$n_species_raref_log_std_std))
z2 <- seq(-2, 2)

newdf <- expand.grid(n_species_raref_log_std_std = z1, precip_std_std = z2)

mois_int <- ggplot() + 
  geom_point(data = int_df, 
    aes(x = n_species_raref_log_std_std, y = agb_ha_log_std_std, fill = precip_std_std),
    shape = 21) + 
  scale_fill_viridis_c(name="Moisture\navailability (SD)", option = "C", breaks = z2, limits = c(-2,2)) + 
  new_scale("fill") + 
  stat_smooth(data=transform(newdf, 
    yp=predict(mois_div_int_mod, newdf)), 
    aes(y=yp, x=n_species_raref_log_std_std, color=factor(precip_std_std)), 
    method="lm") + 
  scale_colour_viridis_d(name=, option = "C", guide = FALSE) + 
  labs(x="Tree species diversity", y="AGB") + 
  theme_classic() + 
  theme(legend.position = c(0.9,0.3))

pdf("img/mois_int.pdf", width = 8, height = 5)
mois_int
dev.off()

soil_div_int_mod <- lm(agb_ha_log_std_std ~ n_species_raref_log_std_std + soil_c_log_std_std + 
    n_species_raref_log_std_std*soil_c_log_std_std , data = int_df)

z1 <- seq(min(int_df$n_species_raref_log_std_std), max(int_df$n_species_raref_log_std_std))
z2 <- seq(-2, 2)

newdf <- expand.grid(n_species_raref_log_std_std = z1, soil_c_log_std_std = z2)

soil_int <- ggplot() + 
  geom_point(data = int_df, 
    aes(x = n_species_raref_log_std_std, y = agb_ha_log_std_std, fill = soil_c_log_std_std),
    shape = 21) + 
  scale_fill_viridis_c(name="Soil\nfertility (SD)", option = "C", breaks = z2, limits = c(-2,2)) + 
  new_scale("fill") + 
  stat_smooth(data=transform(newdf, 
    yp=predict(soil_div_int_mod, newdf)), 
    aes(y=yp, x=n_species_raref_log_std_std, color=factor(soil_c_log_std_std)), 
    method="lm") + 
  scale_colour_viridis_d(name=, option = "C", guide = FALSE) + 
  labs(x="Tree species diversity", y="AGB") + 
  theme_classic() + 
  theme(legend.position = c(0.9,0.3))

pdf("img/soil_int.pdf", width = 8, height = 5)
soil_int
dev.off()

test <- summary(mois_div_int_mod)

fileConn <- file("include/moder_coef.tex")
writeLines(
  c(
    paste0("\\newcommand{\\moderp}{", "$\\beta =$ ", round(summary(mois_div_int_mod)$coefficients[4,1], digits = 2), 
      ", t(", summary(mois_div_int_mod)$df[2], ") = ", round(summary(mois_div_int_mod)$coefficients[4,3], digits = 2),", ", 
      p_format(summary(mois_div_int_mod)$coefficients[4,4]), "}"),
    paste0("\\newcommand{\\moders}{", "$\\beta =$ ", round(summary(soil_div_int_mod)$coefficients[4,1], digits = 2), 
      ", t(", summary(soil_div_int_mod)$df[2], ") = ", round(summary(soil_div_int_mod)$coefficients[4,3], digits = 2),", ", 
      p_format(summary(soil_div_int_mod)$coefficients[4,4]), "}")),
    fileConn)
close(fileConn)


# Summary of models
fileConn <- file("include/mois_div_int_mod.tex")
writeLines(stargazer(mois_div_int_mod, notes.label = "", label = "mois_div_int_mod", notes.append = FALSE),
  fileConn)
close(fileConn)

fileConn <- file("include/soil_div_int_mod.tex")
writeLines(stargazer(soil_div_int_mod, notes.label = "", label = "soil_div_int_mod",  notes.append = FALSE),
  fileConn)
close(fileConn)


