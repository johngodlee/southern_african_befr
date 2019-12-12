# Generate productivity estimates for each plot in the SEOSAW network

.PHONY: all plots_prod_est plots_data_lat

# Variables 
SEOSAW=../data/seosaw_plot_summary5Apr2019.Rdata
EXTRAC=extracted_data

all: plots_prod_est

# Run productivity model
plots_prod_est: plots_data_lat src/ACM_GPP_ET_R_interface.f90 src/ACM_GPP_ET.f90 acm_gpp.R 
	Rscript acm_gpp.R

# Add latitude values
plots_data_lat: lat_vec.txt plot_lat.sh
	./plot_lat.sh

# Split data into per plot parts
lat_vec.txt: plot_split.R $(EXTRAC)/all_data_compiled.csv
	Rscript plot_split.R

# Compile data
$(EXTRAC)/all_data_compiled.csv: merge_extracted.R $(EXTRAC)/extracted_ecmwf_all_daily.Rdata $(EXTRAC)/soil_data_plots.csv 	
	Rscript merge_extracted.R

# Extract data from raw files
## ECMWF
$(EXTRAC)/extracted_ecmwf_all_daily.Rdata: ecmwf_extract.R raw_data/era5/*anc $(SEOSAW)
	Rscript ecmwf_extract.R

$(EXTRAC)/soil_data_plots.csv: soil_extract.R $(SEOSAW) 
	Rscript soil_extract.R
