# Identifying plots with tree species compositional outliers so they can be removed from the dataset 
# John Godlee (johngodlee@gmail.com)
# 2019_11_05

# Preamble ----

# Remove old crap
rm(list=ls())
#dev.off()

# Set working directory to the location of the source file
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)
library(ggplot2)
library(dave)

# Import data ----
plot_data <- readRDS("data/plot_data_fil_agg_norm_std.rds")

plotcode_plot_group_lookup <- readRDS("data/plotcode_plot_group_lookup.rds")

stem_ab_mat <- readRDS("data/stems_ab_mat.rds")

# Run outlier detection ----
# Identify outlier threshold for ~5% of plots
# outlier_list <- list()
# 
# for(i in 1:100){
#   x <- seq(from = 0.005, to = 0.5, by = 0.005)[i]
#   outlier_list[[i]] <- outlier(stem_ab_mat, thresh = x, y = 0.5)
# }
# 
# # Save to data file
# saveRDS(outlier_list, file = "data/outlier_list.rds")
outlier_list <- readRDS("data/outlier_list.rds")

# Create plot of threshold values and number of outliers
thresh <- sapply(outlier_list, function(x){
  x$threshold
})

outlier_num <- sapply(outlier_list, function(x){
  x$olddim[1] - x$newdim[1]
})

outlier_df <- data.frame(thresh, outlier_num)

thresh_0.05 <- as.numeric(unname(outlier_df[
  which.min(abs(outlier_df$outlier_num - (nrow(stem_ab_mat) * 0.05))), ]))

outlier_thresh <- ggplot(outlier_df, aes(x = thresh, y = outlier_num)) + 
  geom_smooth(span = 0.2, colour = "#B00000") + 
  geom_point() +
  geom_vline(aes(xintercept = thresh_0.05[1])) + 
  geom_hline(aes(yintercept =  thresh_0.05[2])) + 
  geom_label(aes(x = thresh_0.05[1], y = 150, label = thresh_0.05[1])) + 
  geom_label(aes(y = thresh_0.05[2], x = 0.15, label = thresh_0.05[2])) + 
  theme_classic() + 
  labs(x = "Distance threshold", y = "Number of outliers")

pdf(file = "img/outlier_thresh.pdf", width = 10, height = 8)
outlier_thresh
dev.off()

n_outliers <- thresh_0.05[2]

fileConn <- file(paste0("include/n_outliers.tex"))
writeLines(paste0("\\newcommand{\\noutliers}{", n_outliers, "}"),
  fileConn)
close(fileConn)

# Exclude outliers from plot_data and save ----
# Get plot names of outliers at 0.05 threshold
outlier_best <- outlier_list[[which(thresh == thresh_0.05[1])]]$new.data

# This outlier removal has been REMOVED from analysis
plot_data_clean <- plot_data #%>%
  #filter(plot_group %in% row.names(outlier_best))

# Save plot data
saveRDS(plot_data_clean, "data/plot_data_fil_agg_norm_std_outlier.rds")

