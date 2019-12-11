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
library(tidyr)
library(dave)

source("full_best.R")

# Import data ----
plot_data <- read.csv(paste0("data/plot_data_fil_agg", ext, "_norm_std.csv"))

plotcode_plot_group_lookup <- read.csv("data/plotcode_plot_group_lookup.csv")
load("data/clean_input_data.Rdata")

# Filter stem data to only include suitable plots and living stems
##; Suitable plots identified in data_collation.R
s_fil <- s %>%
  left_join(., plotcode_plot_group_lookup, by = c("plotcode" = "plotcode_vec")) %>%
  inner_join(., data.frame(plot_group = plot_data$plot_group), 
    by = c("plot_group" = "plot_group")) %>%
  filter(alive %in% c("A", NA),
    !is.na(gen_sp))

# Create community composition matrix
stem_ab_mat <- s_fil %>%
  group_by(plot_group, gen_sp, .drop = FALSE) %>%
  tally() %>%
  spread(gen_sp, n, fill = 0) %>%
  ungroup() %>%
  data.frame() 

# Set rownames
rownames(stem_ab_mat) <- stem_ab_mat$plot_group

stem_ab_mat <- dplyr::select(stem_ab_mat, -plot_group)

#vegan::metaMDS(stem_ab_mat) 
# No convergent solutions - best solution after 20 tries


# Identify outlier threshold for ~5% of plots
outlier_list <- list()

for(i in 1:100){
  x <- seq(from = 0.005, to = 0.5, by = 0.005)[i]
  outlier_list[[i]] <- outlier(stem_ab_mat, thresh = x, y = 0.5)
}

# Save to data file
saveRDS(outlier_list, file = paste0("data/outlier_list", ext, ".rds"))

outlier_list <- readRDS(paste0("data/outlier_list", ext, ".rds"))

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

pdf(file = paste0("img/outlier_thresh", ext, ".pdf"), width = 10, height = 8)
outlier_thresh
dev.off()

# Plot Principle COordinate Analysis (PCOA) of outliers with other points
outlier_best <- outlier_list[[as.numeric(rownames(outlier_df[
  which.min(abs(outlier_df$outlier_num - (nrow(stem_ab_mat) * 0.05))), 
  ]))]]

outlier_best_plot <- data.frame(outlier_best$pco.points, 
  dist = outlier_best$neigh.dist)

outlier_best_plot <- outlier_best_plot %>%
  mutate(outlier = case_when(
    dist >= thresh_0.05[1] ~ TRUE,
    TRUE ~ FALSE
  ))

names(outlier_best_plot) <- c("x", "y", "dist", "outlier")

pcoa <- ggplot(outlier_best_plot, aes(x = x, y = y)) + 
  geom_point(aes(fill = outlier, alpha = outlier), 
    colour = "black", shape = 21, size = 3) + 
  scale_fill_manual(name = "Outlier", values = c("#2B2B2B", "#D43939")) + 
  scale_alpha_manual(name = "Outlier", values = c(0.5, 1)) +
  labs(x = "PCOA 1", y = "PCOA 2") + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.2),
    legend.background = element_rect(fill = "#E6E6E6", colour = "black"))

pdf(file = paste0("img/pcoa_outlier", ext, ".pdf"), width = 10, height = 10)
pcoa
dev.off()

n_outliers <- thresh_0.05[2]

fileConn <- file(paste0("output/include/n_outliers.tex"))
writeLines(paste0("\\newcommand{\\noutliers}{", n_outliers, "}"),
  fileConn)
close(fileConn)


# Exclude outliers from plot_data and save
plot_data_clean <- plot_data %>%
  filter(plot_group %in% row.names(outlier_best$new.data))

# Save plot data
write.csv(plot_data_clean, 
  paste0("data/plot_data_fil_agg", ext, "_norm_std_outlier.csv"), 
  row.names = FALSE)





