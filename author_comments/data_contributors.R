# Who is a data contributor?
# John Godlee (johngodlee@gmail.com)
# 2020-02-07

# Preamble ----

# Remove old crap 
rm(list = ls())
# dev.off()

# Set working directory

# Packages
library(dplyr)

# Load data ----
plots <- readRDS("data/plot_data_fil_agg_norm_std_outlier.rds")

# Who is a contributor and for which plots?
unique(plots$pi)

plots %>%
  dplyr::select(pi, plotcode) %>%
  group_by(pi) %>%
  summarise(plotcode = first(plotcode))

plots %>%
  filter(pi == "Y") %>%
  pull(
##' SHD - belongs to ???

plots %>% 
  filter(pi == "FRIM")
##' MCL - belongs to Anderson Muchawona
