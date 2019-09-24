# Split compiled data into plot level files 
# John Godlee (johngodlee@gmail.com)
# 2019_09_20

# Preamble ----

# Remove old crap 
rm(list = ls())
# dev.off()

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(dplyr)

# Load data 
df <- read.csv("all_data_compiled.csv")

# Format data like Luke's data
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df$doy <- format(df$date, format = "%j")
df$min_T_oC <- df$min_temp
df$max_T_oC <- df$max_temp
df$swrad_MJ_day <- df$total_net_sol
df$rainfall_mmday <- df$precip_trmm
df$wind_ms <- 3.23
df$VPD_kPa <- 1
df$co2 <- 400
df$lai <- 0.469393245
df$latitude <- df$lat

df_clean <- df %>%
  select(plot_match, latitude, doy, min_T_oC, max_T_oC, swrad_MJ_day, lai, rainfall_mmday,
    wind_ms, VPD_kPa, co2)

# Split data
df_list <- split(df_clean, df_clean$plot_match)

# Extract latitude and save as text file
lat_vec <- as.character(unname(sapply(df_list, function(x){mean(x$latitude)})))

fileConn <- file("lat_vec.txt")
writeLines(lat_vec, fileConn)
close(fileConn)

# Save each element as a CSV
for(i in 1:length(df_list)){
  csv <- df_list[[i]][,3:11]
  write.csv(csv, paste0("../inputs/ACM_GPP_ET_", names(df_list[i]), ".csv"), row.names = FALSE)
}
