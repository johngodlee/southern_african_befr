# Plot Principle Coordinate Analysis (PCOA) of outliers with other points
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