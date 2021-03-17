# Which species are present in the TRY database?
# John Godlee (johngodlee@gmail.com)
# 2021-01-25

# Import data
try_sp <- read.table("data/try/try_species_list.txt", header = TRUE, sep = "\t")

seosaw_stems <- read.csv("~/git_proj/seosaw_data/data_out/v2.10/stems_v2.10.csv")

# Extract species from SEOSAW
seosaw_sp <- unique(seosaw_stems$species_name_clean)

# Match SEOSAW species with TRY database
try_match <- try_sp[try_sp$AccSpeciesName %in% seosaw_sp,]

# Which species aren't matched
seosaw_no_match <- seosaw_sp[!seosaw_sp %in% try_sp$AccSpeciesName]

# Check all species either matched or not matched
stopifnot(length(seosaw_no_match) + nrow(try_match) == nrow(seosaw_sp))

# Write matched species IDs to file for upload to TRY website
writeLines(paste(try_match$AccSpeciesID, collapse = ","), 
  "data/try_seosaw_species_id.txt")

