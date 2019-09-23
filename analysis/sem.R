# Structural Equation Models for Chapter 1 - Regional analysis of the factors affecting woody AGB and productivity
# John Godlee (johngodlee@gmail.com)
# 2019_09_20

# Notes for interpreting SEM models
##' Double headed arrows show covariances
##' Single headed arrows show causal relationships
##' Exogenous variables (i.e. independent variables) should have a measurement error (rounded double ended arrows) of 1.00. Exogenous variables may have covariance


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

# Import data ----

# Aggregated and filtered data
sem_data <- read.csv("data/plot_data_fil_agg_norm_std_outlier.csv")


# A simple path model ----
##' Factors affecting AGB, 
##' no interactions or latent variables

path_model_simple_spec <- "
# Factors affecting diversity
shannon_cube_std ~ fire_return_mean_log_std
shannon_cube_std ~ ocdens_std
shannon_cube_std ~ aridity_index_std

# Intermediate factor of covariance of height
cov_height_std ~ shannon_cube_std

# Factors affecting biomass
bchave_log_std ~ cov_height_std
bchave_log_std ~ shannon_cube_std
bchave_log_std ~ fire_return_mean_log_std
bchave_log_std ~ ocdens_std
bchave_log_std ~ aridity_index_std
"

path_model_simple_fit <- cfa(path_model_simple_spec, data = sem_data)

sink("output/path_model_simple_fit.txt")
print(summary(path_model_simple_fit, fit.measures = TRUE))
sink()

pdf(file = "img/path_model_simple.pdf", width = 12, height = 8)
semPaths(path_model_simple_fit ,'mod', "est", 
  layout = "tree", curvature = 1, residuals = FALSE, nCharNodes = 0,
  label.cex = 2)
dev.off()

# Environmental path model ----
##' Only the effects of climate and environment on biomass, 
##' no tree diversity factors
##' No latent variables

env_model_simple_spec <- "
# Factors affecting biomass
bchave_log_std ~ fire_return_mean_log_std
bchave_log_std ~ ocdens_std
bchave_log_std ~ aridity_index_std
"

env_model_simple_fit <- cfa(env_model_simple_spec, data = sem_data)

sink("output/env_model_simple_fit.txt")
print(summary(env_model_simple_fit, fit.measures = TRUE))
sink()

pdf(file = "img/env_model_simple.pdf", width = 12, height = 8)
semPaths(env_model_simple_fit ,'mod', "est", 
  layout = "tree", curvature = 1, residuals = FALSE, nCharNodes = 0,
  label.cex = 2)
dev.off()

# Full latent variable model ----
##' Environmental and biodiversity variables
##' Latent constructs
##' Mediation 

full_latent_mod_spec <- "
# Latent vars
aridity          =~ aridity_index_std
fire             =~ fire_return_mean_log_std
div              =~ shannon_cube_std
struc =~ cov_height_std + cov_dbh_std
biomass          =~ bchave_log_std
soil             =~ ocdens_std

# Regressions - path analysis
## Structural complexity
struc ~ fire
struc ~ aridity
struc ~ a*div
struc ~ soil

## Biomass
biomass ~ b*struc
biomass ~ soil
biomass ~ fire
biomass ~ aridity
biomass ~ c*div

# Explicitly model direct and indirect effects
biomass_div_via_struc := a*b
biomass_div_total := c + (a*b)
"

full_latent_mod_fit <- cfa(full_latent_mod_spec, data = sem_data)

sink("output/full_latent_mod_fit.txt")
print(summary(full_latent_mod_fit, fit.measures = TRUE))
sink()

pdf(file = "img/full_latent_mod.pdf", width = 12, height = 8)
semPaths(full_latent_mod_fit ,'mod', "est", 
  layout = "tree", curvature = 1, residuals = FALSE, nCharNodes = 0,
  label.cex = 2)
dev.off()


# Compare models across vegetation clusters ----
##' Not working so far
measEq.syntax(configural.model = path_model_simple_spec, data=sem_data, group="clust5")
