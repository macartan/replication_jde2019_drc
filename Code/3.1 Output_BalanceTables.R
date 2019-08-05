
# Output .tex files


########## Balance on Tuungane

balvars <-     c("Distance from major urban center", 
                 "Distance to village mine",
                 "Presence of minerals",
                 "Presence of infrastructure in 2006", 
                 "Former chief enthroned by popular choice",
                 "In-migration in 2006", 
                 "Age")

T_Balance <- rbind(
  "\\begin{tabular}{lcccc}",
  "	&	Control	&	\\textit{Tuungane}	&	d-stat	&	N	 \\\\ \\hline \\hline",
  #  "\\mc{5}{c}{\\textbf{Village level}} \\\\ \\hline",
  mat_to_tex(balance.table, rownames = balvars),
  #  "\\hline \\mc{5}{c}{\\textbf{Individual level}} \\\\ \\hline",
  "\\hline \\hline", 
"\\label{table_balance}",
"\\end{tabular}",
"\\begin{flushleft}\\textit{Notes:} $d$-statistic is the difference in (weighted) means expressed in (weighted) standard deviations of the control group outcome.\\end{flushleft}"
)

########## Balance on RAPID

# Balance Table for RAPID
T_Balance_RAPID <- rbind(
  "\\begin{tabular}{lcccc}",
  "	&	Control	&	\\textit{RAPID}	&	d-stat	&	N	 \\\\ \\hline \\hline",
  #  "\\mc{5}{c}{\\textbf{Village level}} \\\\ \\hline",
  mat_to_tex(balance.table.RAPID, rownames = balvars),
  #  "\\hline \\mc{5}{c}{\\textbf{Individual level}} \\\\ \\hline",
  "\\hline \\hline", 
  "\\label{table_balance}",
  "\\end{tabular}",
  "\\begin{flushleft}\\textit{Notes:} $d$-statistic is the difference in (weighted) means expressed in (weighted) standard deviations of the control group outcome.\\end{flushleft}"
  
)

## END ##
