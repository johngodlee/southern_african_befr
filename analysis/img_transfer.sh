#!/bin/bash

OUT="../manuscript/img/"
INC="../manuscript/include/"

# Transfer images from analysis to manuscript

cp img/plot_loc.pdf $OUT

cp img/temp_precip_hull.pdf $OUT

cp img/struc_model_slopes_clust_all.pdf $OUT

cp img/pcoa_outlier.pdf $OUT

cp img/full_latent_model_slopes.pdf $OUT

cp img/corr_mat.pdf $OUT

cp img/biomass_clim_lm_clust.pdf $OUT

cp img/pcoa.pdf $OUT

# Transfer tables from analysis to manuscript

cp output/include/struc_model_fit_clust_stats.tex $INC

