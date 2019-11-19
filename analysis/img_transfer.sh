#!/bin/bash

OUT="../manuscript/img/"
INC="../manuscript/include/"
STRUC_HEAD="{Cluster} \& {Params.} \& {n} \& {\$\\\\chi\^{2}\$} \& {DoF} \& {CFI} \& {TLI} \& {LogLik} \& {AIC} \& {RMSEA} \& {SRMR} \& {\$R\^{2}\$ AGB} \\\\\\\\"

# Transfer images from analysis to manuscript


cp img/plot_loc.pdf $OUT

cp img/temp_precip_hull.pdf $OUT

cp img/struc_model_slopes.pdf $OUT

cp img/struc_model_slopes_all.pdf $OUT

cp img/pcoa_outlier.pdf $OUT

cp img/full_model_slopes.pdf $OUT

cp img/full_model_slopes_all.pdf $OUT

cp img/corr_mat.pdf $OUT

cp img/biomass_clim_lm_clust.pdf $OUT

cp img/clust_map.pdf $OUT

cp img/struc_mod.pdf $OUT

cp img/full_mod.pdf $OUT

cp img/sem_struc_stems_ha.pdf $OUT

cp img/soil_int.pdf $OUT 

cp img/mois_int.pdf $OUT

cp img/histogram_raw_obs.pdf $OUT

cp img/histogram_trans_obs.pdf $OUT

cp sem_path_diag/con_mod.pdf $OUT

cp output/include/n_plots.tex $INC

cp output/include/n_outliers.tex $INC

cp output/include/hull_cover.tex $INC

cp output/include/path_coef_full.tex $INC

cp output/include/path_coef_struc.tex $INC

cp sem_path_diag/path_diagram_*.tex output/include/

cp data_clean_flow/*.tex output/include/

cp output/include/data_clean_flow.tex $INC

cp output/include/path_diagram_struc.tex $INC

cp output/include/path_diagram_full.tex $INC

cp output/include/path_diagram_concept.tex $INC


# Transfer tables from analysis to manuscript

##  Structural model fit
cp output/include/struc_model_fit_clust_stats.tex $INC

sed -i 's/\$//g' "${INC}struc_model_fit_clust_stats.tex"

sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}struc_model_fit_clust_stats.tex"

sed -i "10s/.*/${STRUC_HEAD}/" "${INC}struc_model_fit_clust_stats.tex"

sed -i 's/caption{}/caption{Model fit statistics for SEMs investigating the effects of tree diversity and stem density on AGB (\\autoref{struc_mod}).}/g' "${INC}struc_model_fit_clust_stats.tex" 

## Full model fit
cp output/include/full_model_fit_clust_stats.tex $INC

sed -i 's/\$//g' "${INC}full_model_fit_clust_stats.tex"

sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}full_model_fit_clust_stats.tex"

sed -i "10s/.*/${STRUC_HEAD}/" "${INC}full_model_fit_clust_stats.tex"


## Biogeographical cluster description
cp output/include/clust_summ.tex $INC

sed -i 's/ccccc/clccc/g' "${INC}clust_summ.tex"

sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}clust_summ.tex"

sed -i '12,16s/&/& \\begin\{tabular\}[l]\{@\{\}l@\{\}l@\{\}\}/' "${INC}clust_summ.tex" 

sed -i '12,16s/&/\\end\{tabular\} &/2' "${INC}clust_summ.tex"

sed -i '12,16s/,/ \\\\/g' "${INC}clust_summ.tex"

sed -i '12,16s/+/$\\pm$/g' "${INC}clust_summ.tex"

sed -i -r '12,16s/\w+ \w+\.*/\\textit\{&\}/g' "${INC}clust_summ.tex"

sed -i '12,16s/spp\.\}/\}spp./g' "${INC}clust_summ.tex"

sed -i '10s/.*/{Cluster} \& {Indicator species} \& {Species richness} \& {N plots} \& {Stems ha\\textsuperscript{-1}} \& {AGB (t ha\\textsuperscript{-1})} \\\\/' "${INC}clust_summ.tex"

sed -i '13,16i\\\hline' "${INC}clust_summ.tex"

sed -i 's/caption{}/caption{Description of the biogeographical clusters (C1-C5) to which each plot in the study was assigned. Indicator species were generated using Dufrene-Legendre indicator species analysis \\citep{Dufrene1997} implemented with \\texttt{indval()} from the \\texttt{labdsv} R package \\citep{labdsv}. Numeric values of species richness, stems ha\\textsuperscript{-1} and AGB are medians and interquartile ranges.}/g' "${INC}clust_summ.tex" 

## Correlation coefficients table
cp output/include/corr_ci_tab.tex $INC

sed -i '10s/.*/{X} \& {Y} \& {$\\rho$} \& {lower 95\\% CI} \& {upper 95\\% CI} \& {n} \& {Prob.} \\\\/' "${INC}corr_ci_tab.tex"

sed -i 's/caption{}/caption{Table of correlation fit statistics for each pairwise Pearson correlation test of observed variables used in Structural Equation Models.}/g' "${INC}corr_ci_tab.tex" 

sed -i '4s/.*/\\begin{longtable}[H]{llccccc}/' "${INC}corr_ci_tab.tex"

sed -i 's/\\end{table}/\\end{longtable}/g' "${INC}corr_ci_tab.tex"

sed -i '/\\begin{tabular}/d' "${INC}corr_ci_tab.tex" 
sed -i '/\\end{tabular}/d' "${INC}corr_ci_tab.tex" 

## Moderation models

cp output/include/mois_div_int_mod.tex $INC 

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

cp output/include/soil_div_int_mod.tex $INC 

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
