#!/bin/bash
{

IMG="manuscript/img/"
INC="manuscript/include/"

# Remove intermediate objects

# Run data compilation
Rscript scripts/temp_precip.R
Rscript scripts/soil_carbon.R
Rscript scripts/data_clean.R
Rscript scripts/standardise.R
Rscript scripts/composition.R
Rscript scripts/descrip.R
Rscript scripts/sem.R

# Transfer images to manuscript

cp img/clust_map.pdf $IMG
cp img/corr_mat.pdf $IMG
cp img/struc_model_slopes_all.pdf $IMG
cp img/sem_struc_stems_ha.pdf $IMG
cp img/mois_int.pdf $IMG
cp img/soil_int.pdf $IMG

# Transfer snippets to manuscript

cp include/n_plots.tex $INC
cp include/n_outliers.tex $INC
cp include/hull_cover.tex $INC
cp include/path_coef_struc.tex $INC
cp include/path_coef_full.tex $INC
cp include/corr_coef.tex $INC
cp include/moder_coef.tex $INC
cp include/dens_stats.tex $INC
cp include/perc_small_agb.tex $INC

# Transfer path diagrams to manuscript 

cp include/path_diagram_concept.tex $INC
cp include/path_diagram_struc.tex $INC
cp include/path_diagram_full.tex $INC

cp include/data_clean_flow.tex $INC

# Transfer tables to manuscript

cp include/struc_model_fit_clust_stats.tex $INC
cp include/clust_summ.tex $INC
cp include/corr_ci_tab.tex $INC
cp include/mois_div_int_mod.tex $INC
cp include/soil.tex $INC

# Edit tables

## Structural model fit statistics - struc_model_fit_clust_stats.tex
sed -i 's/\$//g' "${INC}struc_model_fit_clust_stats.tex"
sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}struc_model_fit_clust_stats.tex"
sed -i "10s/.*/{Cluster} \& {n} \& {\$\\\\chi\^{2}\$} \& {DoF} \& {CFI} \& {TLI} \& {LogLik} \& {RMSEA} \& {\$R\^{2}\$ AGB} \\\\\\\\/" "${INC}struc_model_fit_clust_stats.tex"
sed -i 's/caption{}/caption{Model fit statistics for SEMs investigating the effects of tree diversity and stem density on AGB (\\autoref{struc_mod}).}/g' "${INC}struc_model_fit_clust_stats.tex" 

## Cluster description table - clust_summ.tex
sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}clust_summ.tex"
sed -i '12,16s/&/& \\begin\{tabular\}[c]\{@\{\}c@\{\}c@\{\}\}/' "${INC}clust_summ.tex" 
sed -i '12,16s/&/\\end\{tabular\} &/2' "${INC}clust_summ.tex"
sed -i '12,16s/&/& \\begin\{tabular\}[c]\{@\{\}c@\{\}c@\{\}\}/2' "${INC}clust_summ.tex" 
sed -i '12,16s/&/\\end\{tabular\} &/3' "${INC}clust_summ.tex"
sed -i '12,16s/,/ \\\\/g' "${INC}clust_summ.tex"
sed -i '12,16s/+/$\\pm$/g' "${INC}clust_summ.tex"
sed -i -r '12,16s/\w+ \w+\.*/\\textit\{&\}/g' "${INC}clust_summ.tex"
sed -i '12,16s/spp\.\}/\}spp./g' "${INC}clust_summ.tex"
sed -i '10s/.*/{Cluster} \& {Dominant species} \& {Indicator species} \& {N plots} \& {Species rich.} \& {Stems ha\\textsuperscript{-1}} \& {AGB (t ha\\textsuperscript{-1})} \\\\/' "${INC}clust_summ.tex"
sed -i '13,16i\\\hline' "${INC}clust_summ.tex"
sed -i 's/caption{}/caption{Description of the biogeographical clusters (C1-C5) to which each plot in the study was assigned. Indicator species were generated using Dufrene-Legendre indicator species analysis \\citep{Dufrene1997} implemented with \\texttt{indval()} from the \\texttt{labdsv} R package \\citep{labdsv} and represent species which define the given cluster. Dominant species were identified by choosing the species with the largest AGB contribution within each cluster. Numeric values of species richness, stems ha\\textsuperscript{-1} and AGB are medians and interquartile ranges (75th percentile - 25th percentile).}/g' "${INC}clust_summ.tex" 

## Full table of pairwise correlations - corr_ci_tab.tex
sed -i '10s/.*/{X} \& {Y} \& {r} \& {lower 95\\% CI} \& {upper 95\\% CI} \& {n} \& {Prob.} \\\\/' "${INC}corr_ci_tab.tex"
sed -i 's/caption{}/caption{Table of correlation fit statistics for each pairwise Pearson correlation test of observed variables used in Structural Equation Models.}/g' "${INC}corr_ci_tab.tex" 
sed -i '4s/.*/\\begin{longtable}[H]{llccccc}/' "${INC}corr_ci_tab.tex"
sed -i 's/\\end{table}/\\end{longtable}/g' "${INC}corr_ci_tab.tex"
sed -i '/\\begin{tabular}/d' "${INC}corr_ci_tab.tex" 
sed -i '/\\end{tabular}/d' "${INC}corr_ci_tab.tex" 

## Interaction regression table for moisture - mois_div_int_mod.tex
sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}mois_div_int_mod.tex"
sed -i '/\& \\\\/d' "${INC}mois_div_int_mod.tex"
sed -i 's/sp\\_rich\\_raref\\_log\\_std\\_std:total\\_precip\\_std\\_std/Tree sp. div. * Moisture avail./g' "${INC}mois_div_int_mod.tex"
sed -i 's/sp\\_rich\\_raref\\_log\\_std\\_std/Tree sp. div./g' "${INC}mois_div_int_mod.tex"
sed -i 's/total\\_precip\\_std\\_std/Moisture avail./g' "${INC}mois_div_int_mod.tex"
sed -i '/Dependent variable/d' "${INC}mois_div_int_mod.tex" 
sed -i '/\\cline{2-2}/d' "${INC}mois_div_int_mod.tex" 
sed -i '12s/\\\\//g' "${INC}mois_div_int_mod.tex"
sed -i '13s/\&//g' "${INC}mois_div_int_mod.tex"
sed -i '14s/\\\\//g' "${INC}mois_div_int_mod.tex"
sed -i '15s/\&//g' "${INC}mois_div_int_mod.tex"
sed -i '16s/\\\\//g' "${INC}mois_div_int_mod.tex"
sed -i '17s/\&//g' "${INC}mois_div_int_mod.tex"
sed -i '18s/\\\\//g' "${INC}mois_div_int_mod.tex"
sed -i '19s/\&//g' "${INC}mois_div_int_mod.tex"
sed -i '10s/\\\\\[-1\.8ex\]//g' "${INC}mois_div_int_mod.tex"
sed -i '/Adjusted/d' "${INC}mois_div_int_mod.tex" 
sed -i '24s/(df.*$/\\\\/g' "${INC}mois_div_int_mod.tex" 
sed -i 's/caption{}/caption{Regression fit for a linear multiple refression including the latent variables of moisture availbility, tree species diversity and their interaction term on AGB.}/g' "${INC}mois_div_int_mod.tex" 

## Interaction regression table for soil - soil_div_int_mod.tex
sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}soil_div_int_mod.tex"
sed -i '/\& \\\\/d' "${INC}soil_div_int_mod.tex"
sed -i 's/sp\\_rich\\_raref\\_log\\_std\\_std:ocdens\\_std\\_std/Tree sp. div. * Soil fert./g' "${INC}soil_div_int_mod.tex"
sed -i 's/sp\\_rich\\_raref\\_log\\_std\\_std/Tree sp. div./g' "${INC}soil_div_int_mod.tex"
sed -i 's/ocdens\\_std\\_std/Soil fert./g' "${INC}soil_div_int_mod.tex"
sed -i '/Dependent variable/d' "${INC}soil_div_int_mod.tex" 
sed -i '/\\cline{2-2}/d' "${INC}soil_div_int_mod.tex" 
sed -i '12s/\\\\//g' "${INC}soil_div_int_mod.tex"
sed -i '13s/\&//g' "${INC}soil_div_int_mod.tex"
sed -i '14s/\\\\//g' "${INC}soil_div_int_mod.tex"
sed -i '15s/\&//g' "${INC}soil_div_int_mod.tex"
sed -i '16s/\\\\//g' "${INC}soil_div_int_mod.tex"
sed -i '17s/\&//g' "${INC}soil_div_int_mod.tex"
sed -i '18s/\\\\//g' "${INC}soil_div_int_mod.tex"
sed -i '19s/\&//g' "${INC}soil_div_int_mod.tex"
sed -i '10s/\\\\\[-1\.8ex\]//g' "${INC}soil_div_int_mod.tex"
sed -i '/Adjusted/d' "${INC}soil_div_int_mod.tex" 
sed -i '24s/(df.*$/\\\\/g' "${INC}soil_div_int_mod.tex" 
sed -i 's/caption{}/caption{Regression fit for a linear multiple refression including the latent variables of soil fertility, tree species diversity and their interaction term on AGB.}/g' "${INC}soil_div_int_mod.tex" 

} > stdout.log 2>stderr.log

# Read errors
grep -hnri "error" stdout.log
grep -hnri "error" stderr.log
