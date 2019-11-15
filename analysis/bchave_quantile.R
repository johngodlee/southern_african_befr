# Investigating how the relationship of AGB and species richness varies with size class
# John Godlee (johngodlee@gmail.com)
# 2019_11_13

rm(list = ls())

# Packages 
library(dplyr)
library(ggplot2)

# Import data 
bchave_quantile_df <- read.csv("data/bchave_quantile.csv")

quantile_labs <- c(paste0(seq(from = 0, to = 95, by = 5), "-", seq(from = 5, to = 100, by = 5)))

# Mean AGB per quantile
quantile_gather <- bchave_quantile_df %>%
  gather("quantile", "bchave", -plot_group, -sp_rich, -sp_rich_raref) %>%
  mutate(quantile = factor(quantile))

levels(quantile_gather$quantile) <- quantile_labs

ggplot(quantile_gather, aes(x = sp_rich_raref, y = log(bchave))) + 
  geom_point(aes(colour = quantile), alpha = 0.6) + 
  geom_smooth(method = "lm", colour = "black") + 
  facet_wrap(~quantile) + 
  theme_classic() + 
  theme(legend.position = "none")

# Linear models
mod_list <- list()
for(i in 1:length(bchave_quantile_df[,-1:-3])){
  mod_list[[i]] <- summary(lm(log(bchave_quantile_df[,-1:-3][,i]) ~ sp_rich_raref, data = bchave_quantile_df))
}

mod_summ_df <- data.frame(
  quantile = quantile_labs,
  est = sapply(mod_list, function(x){x$coefficients[2,1]}),
  se = sapply(mod_list, function(x){x$coefficients[2,2]}))

ggplot() + 
  geom_point(data = mod_summ_df, aes(x = quantile, y = est)) + 
  geom_errorbar(data = mod_summ_df, 
    aes(x = quantile, ymin = est - se, ymax = est + se)) + 
  labs(x = "Percentile of tree size\nmeasured by AGB",
    y = "Slope of linear model\n(log(AGB) ~ Species rich.)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(size=12, 
    angle=45, 
    vjust=1, 
    hjust=1))
