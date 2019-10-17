# Merge each extracted rds year file from ECMWF into one dataframe
# John Godlee (johngodlee@gmail.com)
# 2019_09_26

# Preamble ----

# Remove old crap
rm(list = ls())

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages

extrac_names_list <- list.files(paste0(getwd(), "/extracted_data"),
  pattern = "extracted_ecmwf_all_daily_20",
  full.names = TRUE)

extrac_files_list <- list()
for(i in 1:length(extrac_names_list)){
  extrac_files_list[[i]] <- readRDS(extrac_names_list[[i]])
}

extrac_df_list <- lapply(extrac_files_list, function(x){
  do.call(rbind, x)
})

extrac_df <- do.call(rbind, extrac_df_list)

saveRDS(extrac_df, file = "extracted_data/ecmwf_extrac_df.rds")


