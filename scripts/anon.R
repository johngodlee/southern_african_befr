# Create an anonymised dataset for hosting on data repository
# John Godlee (johngodlee@gmail.com)
# 2021-07-09

# Packages
library(dplyr)

# Import data
dat <- readRDS("../data/plot_data_fil_agg_norm_std.rds")

dat_anon <- dat %>% 
  dplyr::select(
    -plot_cluster,
    -longitude_of_centre,
    -latitude_of_centre,
    -clust5, 
    -clust7,
    -temp_seas,
    -starts_with("sd_"),
    -starts_with("var_"),
    -starts_with("mean_"),
    -starts_with("max_"),
    -ends_with("_ha"),
    -ends_with("_std"),
    -ends_with("_log")) %>%
  mutate(plot_id = row_number())

write.csv(dat_anon, "../data/plot_data_anon.csv", row.names = FALSE)
