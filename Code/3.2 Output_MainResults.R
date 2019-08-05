
############################################ CAPTURE

main_table_tex <- main_table
main_table_tex[,4] <- paste0( "(", main_table_tex[,4], ")")

index_col <- rep("No",5)

main_table_tex <- cbind(index_col, main_table_tex)

T2 <- rbind(
  "\\begin{tabular}{lcccccc}",
#  "\\mc{5}{c}{Measures of Capture} \\\\ \\hline",
  "	& Index &	Control	&	Effect	&	(se)	&	N	& Clusters \\\\ \\hline \\hline",
  
  mat_to_tex(main_table_tex[,-3], rownames = capture_var_names),
  #  mat_to_tex(get.results.tab(main.analysis)[1:5,-1], rownames = capture_var_names) ,
  "\\hline \\hline",  
  "\\label{table_main}",
  "\\end{tabular}",
"\\begin{flushleft}\\textit{Notes:} For ``Embezzlement (list experiment)'' and ``Dominance of Chief's Preferences'' we estimate equation (2), 
  and report estimates for $\\beta_2$ in the Control column and estimates for $\\beta_3$ in the Effect column, where \\textit{X} is the sensitive 
  item and the chief, respectively. All analyses employ inverse propensity weights, clustering of standard errors at the level 
  of randomization clusters, and block fixed effects. Cluster column refers to the number of unique village clusters.
  $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$.\\end{flushleft}"
)


############################################ THE OTHER FOUR
# mech_results <- get.results.tab(main.analysis)[6:16,-1]

index <- c("MFI_COMPLAINTS", "MFI_SELECTION",  "MFI_COMPOSITION", "MFI_COMPLAINTS", "MFI_MECHANISMS", "MFI_ACCOUNTING")
index_col <- ifelse(dvs_mech %in% index, "Yes", "No")

mechanisms_table_tex <- mechanisms_table
mechanisms_table_tex[,4] <- paste0( "(", mechanisms_table_tex[,4], ")")
mechanisms_table_tex <- cbind(index_col, mechanisms_table_tex)


T3 <- rbind(
  "\\begin{tabular}{lcccccc}",
  "	&	Index & Control	&	Effect	&	(se)	&	N	& Clusters \\\\ \\hline \\hline",
  "\\mc{5}{l}{\\textbf{Participation}} \\\\ \\hline",
  mat_to_tex(mechanisms_table_tex[1:5,-3], rownames = mech_var_names[1:5]) ,
  "\\hline \\mc{5}{l}{\\textbf{Accountability}} \\\\ \\hline",
  mat_to_tex(mechanisms_table_tex[6:7,-3], rownames = mech_var_names[6:7]) ,
  "\\hline \\mc{5}{l}{\\textbf{Transparency}} \\\\ \\hline",
  mat_to_tex(mechanisms_table_tex[8:10,-3], rownames = mech_var_names[8:10]) ,
  # "\\hline \\mc{5}{l}{\\textbf{Efficiency}} \\\\ \\hline",
  # mat_to_tex(mech_results[10:11,], rownames = mech_var_names[10:11]) ,
  "\\hline \\hline",
  "\\label{table_mech}",
  "\\end{tabular}",
  "\\begin{flushleft}\\textit{Notes:}  Outcome measures in rows 4 to 7 and row 10 are indices and by construction have  zero average in control areas. Deviations from 0 in the control column may arise because we report the weighted average of block average outcomes in control areas, which differs marginally from the overall average. All analyses employ inverse propensity weights, clustering of standard errors at the level 
  of randomization clusters, and block fixed effects. $^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$ \\end{flushleft}"
  
)

## END ##
