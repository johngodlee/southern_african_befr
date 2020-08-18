#!/usr/bin/env sh

IMG="manuscript/img/"
INC="manuscript/include/"

# Run data compilation
Rscript scripts/temp_precip.R
Rscript scripts/data_clean.R
Rscript scripts/standardise.R
Rscript scripts/descrip.R
Rscript scripts/sem.R

# Transfer images to manuscript
cp img/clust_map.pdf $IMG  # Cluster map
cp img/corr_mat.pdf $IMG  # Correlation matrix
cp img/struc_model_slopes_all.pdf $IMG  # SEM slopes - structural model
cp img/full_model_slopes.pdf $IMG  # SEM slopes - full model
cp img/sem_struc_stems_ha.pdf $IMG  # SEM stem density line plots 
cp img/hist_raw.pdf $IMG  # Histogram of variables, raw
cp img/hist_trans.pdf $IMG  # Histogram of variables, transformed
cp img/bivar_lm.pdf $IMG  # Bivariate linear regressions of variables

# Transfer path diagrams to manuscript 
cp img/concept.png $IMG  # Concept path diagram
cp img/struc.png $IMG  # Structural path diagram
cp img/full.png $IMG  # Full model path diagram

# Transfer snippets to manuscript
cp include/n_plots.tex $INC  # \nplots, \ntrees
cp include/hull_cover.tex $INC  # \hullcover
cp include/path_coef_struc.tex $INC  # Structural SEM stats
cp include/path_coef_full.tex $INC  # Full SEM stats
cp include/corr_coef.tex $INC  # Correlation coefficient stats
cp include/dens_stats.tex $INC  # SEM stem density stats (\subn, \subp)
cp include/perc_small_agb.tex $INC  # \percsmallagb
cp include/zam.tex $INC  # Zambia plot clusters

# Transfer tables to manuscript
cp include/clust_summ.tex $INC  # Vegetation type stats
cp include/struc_model_fit_clust_stats.tex $INC  # Structural model fit
cp include/corr_ci_tab.tex $INC  # Correlation matrix statistics

