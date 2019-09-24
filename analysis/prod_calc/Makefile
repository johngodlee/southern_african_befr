# Generate productivity estimates for each plot in the SEOSAW network

.PHONY: all plots_prod_est plots_data_lat

# Variables 
SEOSAW=../data/seosaw_plot_summary5Apr2019.Rdata
COLL=data_collation
EXTRAC=extracted_data
TRMM_RAW=raw_data/trmm/*

all: plots_prod_est

# Run productivity model
plots_prod_est: plots_data_lat src/ACM_GPP_ET_R_interface.f90 src/ACM_GPP_ET.f90 acm_gpp.R 
	Rscript acm_gpp.R

# Add latitude values
plots_data_lat: $(COLL)/lat_vec.txt
	./plot_lat.sh

# Split data into per plot parts
$(COLL)/lat_vec.txt: $(COLL)/plot_split.R $(COLL)/$(EXTRAC/all_data_compiled.csv
	Rscript $(COLL)/plot_split.R

# Compile data
$(EXTRAC)/all_data_compiled.csv: $(COLL)/merge_extracted.R $(COLL)/$(EXTRAC)/extracted_ecmwf_all_daily.Rdata $(COLL)/$(EXTRAC)/extracted_trmm_all.Rdata $(COLL)/$(EXTRAC)/soil_data_plots.csv $(COLL)/$(EXTRAC)/lai_interp_all.Rdata
	Rscript merge_extracted.R

# Extract data from raw files
## ECMWF
$(EXTRAC)/extracted_ecmwf_all_daily.Rdata: $(COLL)/ecmwf_extract.R $(COLL)/raw_data/ecmwf/ecmwf_nc/*.nc $(SEOSAW)
	Rscript $(COLL)/ecmwf_extract.R

$(EXTRAC)/soil_data_plots.csv: $(COLL)/soil_extract.R $(SEOSAW) 
	Rscript $(COLL)/soil_extract.R
