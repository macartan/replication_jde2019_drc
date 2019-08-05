

############################
# Table 6: Robust Analyses: Base; Alternative treatment; Alternative specifications: village, bins, prop.; Spillover effects: D(5km), I(5km), D(20k), I(20k)
############################




# DFS <- list(D_AUDIT_NOTVERI, ABD_INDIV_DIRECT, ABD_INDIV_LIST,
#             ABD_INDIV_BEN, CHIEF_DOM, ABD_VILL_ATTEND_7,
#             AB_DISC, AB_DISC, ABD_VILL_SEL, ABD_VILL_1,
#             ABD_MERGE, ABD_INDIV_COMPLAINTS, ABD_INDIV_KNOWS,
#             ABD_INDIV_SEEKS, D_AUDIT_ACCOUNT, ABD_INDIV_HEALTH)

# mainanalysis <- splice_sds(get.results.tab(main.analysis)[,3:4])

## Create table
CONTENT <- cbind(robust_main_results, robust_alt_treatment, robust_village_level, robust_lott_bins, robust_prop_weight)

T6 <- rbind(
  "\\centering",
  "\\scriptsize",
  "\\begin{tabular}{lc|c|ccc}",
  "	&      &              &	\\mc{3}{c}{Alt. Specifications}  \\\\ ",
  "	& Base &	Alt. Treat. &	Village (weighted) & No block FEs & Village (unweighted) \\\\ ",
  "	& (se) &	(se)        &	(se)    & (se) & (se) \\\\",
  "	& (1) &	(2)        &	(3)    & (4) & (5)  \\\\ \\hline \\hline",
  #  "\\mc{6}{l}{\\textbf{Capture}} \\\\ \\hline",
  mat_to_tex(CONTENT[1:10,], rownames = varnames2[1:10]),
  #  "\\hline \\mc{6}{l}{\\textbf{Participation}} \\\\ \\hline",
  mat_to_tex(CONTENT[11:20,], rownames = varnames2[11:20]),
  #  "\\hline \\mc{6}{l}{\\textbf{Accountability}} \\\\ \\hline",
  mat_to_tex(CONTENT[21:24,], rownames = varnames2[21:24]),
  #  "\\hline \\mc{6}{l}{\\textbf{Transparency}} \\\\ \\hline",
  mat_to_tex(CONTENT[25:28,], rownames = varnames2[25:28]),
  #  "\\hline \\mc{6}{l}{\\textbf{Efficiency}} \\\\ \\hline",
  mat_to_tex(CONTENT[29:30,], rownames = varnames2[29:30]),
  "\\hline \\hline",
  "\\label{table_robust}",
  "\\end{tabular}",
  "\\begin{flushleft}\\textit{Notes:} `Base' corresponds to the results reported in Table \\ref{table_main}. 
  ``Alt. Treat.'' are results using a treatment variable that uses IRC's classification of treatment in cases in which databases disagreed. 
  ``Village (weighted)'' are results in which all variables are aggregated to the village level using individual sampling weights. Estimation includes inverse propensity weights. 
  ``No block FEs'' are results specified in Table \\ref{table_main} but without block fixed effects. 
  ``Village (unweighted)'' are results at the village level aggregated without sampling weights, weighted by inverse propensity weights. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$.\\end{flushleft}" 
)

############################
# TABLE 7: Heterogeneous Analyses: Base; Projects; Committee; Inherited
############################

## Create table

CONTENT <- cbind(robust_main_results, robust_noschools,  robust_nocommittee, robust_inherited)

T7 <- rbind(
  "\\centering",
  "\\scriptsize",
  "\\begin{tabular}{lc|ccc}",
  "	& Base      &   Projects &  Committees &  Inherited    \\\\ ",
  "	& (se)      &   (se)      &  (se)      &  (se)    \\\\ \\hline \\hline",
  #  "\\mc{5}{l}{\\textbf{Capture}} \\\\ \\hline",
  mat_to_tex(CONTENT[1:10,], rownames = varnames2[1:10]),
  #  "\\hline \\mc{6}{l}{\\textbf{Participation}} \\\\ \\hline",
  mat_to_tex(CONTENT[11:20,], rownames = varnames2[11:20]),
  #  "\\hline \\mc{6}{l}{\\textbf{Accountability}} \\\\ \\hline",
  mat_to_tex(CONTENT[21:24,], rownames = varnames2[21:24]),
  #  "\\hline \\mc{6}{l}{\\textbf{Transparency}} \\\\ \\hline",
  mat_to_tex(CONTENT[25:28,], rownames = varnames2[25:28]),
  #  "\\hline \\mc{6}{l}{\\textbf{Efficiency}} \\\\ \\hline",
  mat_to_tex(CONTENT[29:30,], rownames = varnames2[29:30]),
  "\\hline \\hline", 
  "\\label{table_main_gov}",
  "\\end{tabular}",
  "\\begin{flushleft}\\textit{Notes:} This table replicates Table \\ref{table_main} for three subgroups. The column under `Base' are the results as presented in Table \\ref{table_main}. 
  Results under `Projects' are the results for villages without school rooms in 2007, 
  ``Committees'' for those without any village committee or association in 2007, and `Inherited' for those villages where the chief's father's position 
  was inherited. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$. \\end{flushleft}"
)


## END
