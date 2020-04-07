# Generate a list of plots so co-authors know who is involved
# John Godlee (johngodlee@gmail.com)
# 2020-01-27

# Preamble 

# Remove old crap 
rm(list = ls())
# dev.off()

# Set working directory

# Packages
library(dplyr)


# Load data 
plots <- readRDS("data/plot_data_fil_agg_norm_std_outlier.rds")

# Create clean list of plots 
plots_clean <- plots %>%
  dplyr::select(pi, plot_id, plotcode, country)

# Write to .csv 
write.csv(plots_clean, "data/plot_owner_list.csv", row.names = FALSE)

# Summary of PI contribution 
plots_clean %>%
  group_by(pi) %>%
  tally()

plots_clean %>%
  group_by(pi) %>%
  do(head(., n = 5))
