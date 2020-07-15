#!/bin/bash

{

IMG="manuscript/img/"
INC="manuscript/include/"

# Remove intermediate objects

# Run data compilation
#Rscript scripts/temp_precip.R
#Rscript scripts/data_clean.R
#Rscript scripts/standardise.R
#Rscript scripts/descrip.R
#Rscript scripts/sem.R

# Transfer images to manuscript
cp img/clust_map.pdf $IMG  # Cluster map
cp img/corr_mat.pdf $IMG  # Correlation matrix
cp img/struc_model_slopes_all.pdf $IMG  # SEM slopes - structural model
cp img/full_model_slopes.pdf $IMG  # SEM slopes - full model
#cp img/sem_struc_stems_ha.pdf $IMG  # SEM stem density line plots 
cp img/hist_raw.pdf $IMG  # Histogram of variables, raw
cp img/hist_trans.pdf $IMG  # Histogram of variables, transformed
cp img/bivar_lm.pdf $IMG  # Bivariate linear regressions of variables

# Transfer path diagrams to manuscript 
cp sem_path_diag/concept.png $IMG  # Concept path diagram
cp sem_path_diag/struc.png $IMG  # Structural path diagram
cp sem_path_diag/full.png $IMG  # Full model path diagram

# Transfer snippets to manuscript
cp include/n_plots.tex $INC  # \nplots, \ntrees
cp include/hull_cover.tex $INC  # \hullcover
cp include/perc_small_agb.tex $INC  # \percsmallagb
cp include/corr_coef.tex $INC  # Correlation coefficient stats
cp include/path_coef_struc.tex $INC  # Structural SEM stats
cp include/path_coef_full.tex $INC  # Full SEM stats
cp include/dens_stats.tex $INC  # SEM stem density stats (\subn, \subp)

# Transfer tables to manuscript
cp include/struc_model_fit_clust_stats.tex $INC  # Structural model fit
cp include/clust_summ.tex $INC  # Vegetation type stats
cp include/corr_ci_tab.tex $INC  # Correlation matrix statistics

# Edit tables
## Structural model fit statistics - struc_model_fit_clust_stats.tex
sed -i 's/\$//g' "${INC}struc_model_fit_clust_stats.tex"
sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}struc_model_fit_clust_stats.tex"
sed -i "10s/.*/{Cluster} \& {n} \& {\$\\\\chi\^{2}\$} \& {DoF} \& {CFI} \& {TLI} \& {RMSEA} \& {\$R\^{2}\$ AGB} \\\\\\\\/" "${INC}struc_model_fit_clust_stats.tex"
sed -i 's/caption{}/caption{Model fit statistics for SEMs investigating the effects of tree diversity and stem density on AGB (\\autoref{struc_mod}). n = number of plots in cluster, $\\chi^{2}$ = Chi-squared fit statistic, DoF = model degrees of freedom, CFI = Comparative Fit Index, TLI = Tucker-Lewis Index, RMSEA = Root Mean Square Error of Approximation, $R^{2}$ AGB = R-squared of AGB.}/g' "${INC}struc_model_fit_clust_stats.tex" 
sed -i 's/517/523/g' "${INC}struc_model_fit_clust_stats.tex" 
sed -i 's/28/188/g' "${INC}struc_model_fit_clust_stats.tex" 
sed -i 's/46/58/g' "${INC}struc_model_fit_clust_stats.tex" 
sed -i 's/390/466/g' "${INC}struc_model_fit_clust_stats.tex" 
sed -i 's/981/1235/g' "${INC}struc_model_fit_clust_stats.tex" 

## Cluster description table - clust_summ.tex
sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}clust_summ.tex"
sed -i '12,16s/&/& \\begin\{tabular\}[c]\{@\{\}c@\{\}c@\{\}\}/' "${INC}clust_summ.tex" 
sed -i '12,16s/&/\\end\{tabular\} &/2' "${INC}clust_summ.tex"
sed -i '12,16s/&/& \\begin\{tabular\}[c]\{@\{\}c@\{\}c@\{\}\}/2' "${INC}clust_summ.tex" 
sed -i '12,16s/&/\\end\{tabular\} &/3' "${INC}clust_summ.tex"
sed -i '12,16s/,/ \\\\/g' "${INC}clust_summ.tex"
sed -i '12,16s/+/$\\pm$/g' "${INC}clust_summ.tex"
sed -i -r '12,15s/\w+ \w+\.*/\\textit\{&\}/g2' "${INC}clust_summ.tex"
sed -i -r 's/Colophospermum mopane/\\textit\{&\}/g' "${INC}clust_summ.tex"
sed -i '12,16s/spp\.\}/\}spp./g' "${INC}clust_summ.tex"
sed -i '10s/.*/{Cluster} \& {Dominant species} \& {Indicator species} \& {N plots} \& {Species rich.} \& {Stems ha\\textsuperscript{-1}} \& {AGB (t ha\\textsuperscript{-1})} \\\\/' "${INC}clust_summ.tex"
sed -i '13,16i\\\hline' "${INC}clust_summ.tex"
sed -i 's/caption{}/caption{Description of the biogeographical clusters to which each plot in the study was assigned. Indicator species were generated using Dufrene-Legendre indicator species analysis \\citep{Dufrene1997} implemented with \\texttt{indval()} from the \\texttt{labdsv} R package \\citep{labdsv} and represent species which define the given cluster. Dominant species were identified by choosing the species with the largest mean plot level proportional AGB within each cluster. Numeric values of species richness, stems ha\\textsuperscript{-1} and AGB represent medians and interquartile ranges (75th percentile - 25th percentile).}/g' "${INC}clust_summ.tex" 

## Full table of pairwise correlations - corr_ci_tab.tex
sed -i '10s/.*/{X} \& {Y} \& {r} \& {lower 95\\% CI} \& {upper 95\\% CI} \& {n} \& {Prob.} \\\\/' "${INC}corr_ci_tab.tex"
sed -i 's/caption{}/caption{Table of correlation fit statistics for each pairwise Pearson correlation test of observed variables used in Structural Equation Models.}/g' "${INC}corr_ci_tab.tex" 
sed -i '4s/.*/\\begin{longtable}[H]{llccccc}/' "${INC}corr_ci_tab.tex"
sed -i 's/\\end{table}/\\end{longtable}/g' "${INC}corr_ci_tab.tex"
sed -i '/\\begin{tabular}/d' "${INC}corr_ci_tab.tex" 
sed -i '/\\end{tabular}/d' "${INC}corr_ci_tab.tex" 

} > output.log 2>&1
