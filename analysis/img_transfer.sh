#!/bin/bash

OUT="../manuscript/img/"
INC="../manuscript/include/"
STRUC_HEAD="{Cluster} \& {Params.} \& {n} \& {\$\\\\chi\^{2}\$} \& {DoF} \& {CFI} \& {TLI} \& {LogLik} \& {AIC} \& {RMSEA} \& {SRMR} \\\\\\\\"

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

# Transfer tables from analysis to manuscript

cp output/include/struc_model_fit_clust_stats.tex $INC

sed -i 's/\$//g' "${INC}struc_model_fit_clust_stats.tex"

sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}struc_model_fit_clust_stats.tex"

sed -i "10s/.*/${STRUC_HEAD}/" "${INC}struc_model_fit_clust_stats.tex"


cp output/include/full_model_fit_clust_stats.tex $INC

sed -i 's/\$//g' "${INC}full_model_fit_clust_stats.tex"

sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}full_model_fit_clust_stats.tex"

sed -i "10s/.*/${STRUC_HEAD}/" "${INC}full_model_fit_clust_stats.tex"

cp output/include/clust_summ.tex $INC

sed -i 's/ccccc/clccc/g' "${INC}clust_summ.tex"

sed -i 's/\\extracolsep{5pt}/\\extracolsep{0pt}/g' "${INC}clust_summ.tex"

sed -i '12,16s/&/& \\begin\{tabular\}[l]\{@\{\}l@\{\}l@\{\}\}/' "${INC}clust_summ.tex" 

sed -i '12,16s/&/\\end\{tabular\} &/2' "${INC}clust_summ.tex"

sed -i '12,16s/,/ \\\\/g' "${INC}clust_summ.tex"

sed -i '12,16s/+/$\\pm$/g' "${INC}clust_summ.tex"

sed -i '10s/.*/{Cluster} \& {Indicator species} \& {Rarefied species richness} \& {Stems ha\\textsuperscript{-1}} \& {AGB (t ha\\textsuperscript{-1})} \\\\/' "${INC}clust_summ.tex"

sed -i '13,16i\\\hline' "${INC}clust_summ.tex"


